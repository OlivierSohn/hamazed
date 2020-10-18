/*
  This C++ layer on top of the audio-engine defines the
  notion of instruments and uses locks to protect concurrent accesses to
  instruments containers. These locks are acquired
  according to the same global order, everywhere in the code, so as to
  ensure that no deadlock will ever occur.

  These locks are not taken by the audio realtime thread,
  which remains lock-free unless IMJ_AUDIO_MASTERGLOBALLOCK is used.
*/

#include "compiler.prepro.h"
#include "cpp.audio/include/public.h"

#ifdef __cplusplus

namespace imajuscule::audio {
  namespace audioelement {

    using AudioFloat = double;

    // in sync with the corresponding Haskel Enum instance
    enum class OscillatorType {
      Sweep = -2,
      Noise = -1,
      // values >= 0 are in sync with the corresponding Haskell enum
      SinusLoudnessVolumeAdjusted,
      Sinus,
      Saw,
      Square,
      Triangle
    };
    std::ostream & operator << (std::ostream &, OscillatorType);

    template<template<OscillatorType> typename F>
    void foreachOscillatorType() {
      F<OscillatorType::SinusLoudnessVolumeAdjusted>{}();
      F<OscillatorType::Sinus>{}();
      F<OscillatorType::Saw>{}();
      F<OscillatorType::Square>{}();
      F<OscillatorType::Triangle>{}();
      F<OscillatorType::Noise>{}();
      F<OscillatorType::Sweep>{}();
    }

    template<OscillatorType o>
    struct ToFOsc;

    template <>
    struct ToFOsc<OscillatorType::Saw> {
      static constexpr auto convert = FOscillator::SAW;
    };
    template <>
    struct ToFOsc<OscillatorType::Square> {
      static constexpr auto convert = FOscillator::SQUARE;
    };
    template <>
    struct ToFOsc<OscillatorType::Triangle> {
      static constexpr auto convert = FOscillator::TRIANGLE;
    };
    template <>
    struct ToFOsc<OscillatorType::Sinus> {
      // never used
      static constexpr auto convert = FOscillator::SAW;
    };
    template <>
    struct ToFOsc<OscillatorType::SinusLoudnessVolumeAdjusted> {
      // never used
      static constexpr auto convert = FOscillator::SAW;
    };
    template <>
    struct ToFOsc<OscillatorType::Noise> {
      // never used
      static constexpr auto convert = FOscillator::SAW;
    };
    template <>
    struct ToFOsc<OscillatorType::Sweep> {
      // never used
      static constexpr auto convert = FOscillator::SAW;
    };

    template<OscillatorType O, typename FPT>
    struct GenericOscillator {
      using type =
        std::conditional_t<
          O == OscillatorType::SinusLoudnessVolumeAdjusted,
            LoudnessVolumeAdjusted< SineOscillatorAlgo< FPT, eNormalizePolicy::FAST > >,
            std::conditional_t<
              O == OscillatorType::Sinus,
                SineOscillatorAlgo< FPT, eNormalizePolicy::FAST >,
                std::conditional_t<
                  O == OscillatorType::Noise,
                    PinkNoiseAlgo,
                    std::conditional_t<
                      O == OscillatorType::Sweep,
                        FreqSingleRampAlgo< FPT >,
                        FOscillatorAlgo< FPT, ToFOsc<O>::convert >
                    >
                >
            >
        >;
    };

    template<OscillatorType O, typename FPT>
    using genericOscillator = typename GenericOscillator<O, FPT>::type;


    template<OscillatorType O, typename Env>
    struct AudioElementOf {
      using type =
        VolumeAdjusted<
          MultiEnveloped<
            genericOscillator<O, typename Env::FPT>
          , Env
          >
      >;
    };

    template<OscillatorType O, typename Env>
    using audioElementOf = typename AudioElementOf<O, Env>::type;

    template<typename Envelope>
    struct HasNoteOff;

    struct SweepSetup {
      int durationSamples;
      float freq;
      Extremity freq_extremity;
      itp::interpolation interp;

      bool operator ==(SweepSetup const & o) const {
        if (durationSamples != o.durationSamples) {
          return false;
        }
        if (freq != o.freq) {
          return false;
        }
        if (freq_extremity != o.freq_extremity) {
          return false;
        }
        if (interp != o.interp) {
          return false;
        }
        return true;
      }
      bool operator != (SweepSetup const & o) const {
          return !this->operator ==(o);
      }

      std::size_t combine_hash(std::size_t h) const {
        hash_combine(h, durationSamples);
        hash_combine(h, freq);
        hash_combine(h, freq_extremity);
        hash_combine(h, interp);
        return h;
      }
    };

    template<OscillatorType Osc>
    struct ExtraParams {
      std::size_t combine_hash(std::size_t h) const { return h; }
    };

    template<>
    struct ExtraParams<OscillatorType::Sweep> {
      SweepSetup const & sweep;

      std::size_t combine_hash(std::size_t h) const {
        return sweep.combine_hash(h);
      }
    };

    template<OscillatorType Osc>
    struct ExtraParamsSummary {
      ExtraParamsSummary(ExtraParams<Osc> const &)
      {}

      bool operator != (ExtraParamsSummary const & o) const {
          return !this->operator ==(o);
      }
      bool operator == (ExtraParamsSummary const & o) const { return true; }
    };

    template<>
    struct ExtraParamsSummary<OscillatorType::Sweep> {
      ExtraParamsSummary(ExtraParams<OscillatorType::Sweep> const & p)
      : sweep(p.sweep)
      {}

      bool operator != (ExtraParamsSummary const & o) const {
          return !this->operator ==(o);
      }
      bool operator == (ExtraParamsSummary const & o) const {
        if (sweep != o.sweep) {
          return false;
        }
        return true;
      }

    private:
      SweepSetup const sweep;
    };

    template<typename EnvelParamT, typename HarmonicsArray, OscillatorType Osc>
    struct ParamsFor {
      HarmonicsArray const & har;
      EnvelParamT const & env;
      ExtraParams<Osc> params;

      std::size_t hash() const {
        std::size_t h = audioelement::hashHarmonics(har);
        return params.combine_hash(env.combine_hash(h));
      }
    };

    // 'ParamsSummary' is a lightweight version of 'ParamsFor':
    // - If in 'ParamsFor' we have a reference, in 'ParamsSummary' we have the object
    // - If in 'ParamsFor' we have a dynamically allocated object, in 'ParamsSummary' we have the hash of this object
    // - and we have a global hash
    template<typename EnvelParamT, audioelement::OscillatorType Osc>
    struct ParamsSummary {
        template<typename Har>
        ParamsSummary(ParamsFor<EnvelParamT, Har, Osc> const & p)
        : hash_harmonics(audioelement::hashHarmonics(p.har))
        , env(p.env)
        , params_summary(p.params) {
          hash = p.params.combine_hash(env.combine_hash(hash_harmonics));
        }

        bool operator == (ParamsSummary const & o) const {
          if (hash != o.hash) {
            return false;
          }
          if (hash_harmonics != o.hash_harmonics) {
            return false;
          }
          if (env != o.env) {
            return false;
          }
          if (params_summary != o.params_summary) {
            return false;
          }
          return true;
        }

        std::size_t getHash() const { return hash; }

    private:
        // for equality test:
        std::size_t const hash_harmonics;
        EnvelParamT const env;
        ExtraParamsSummary<Osc> const params_summary;

        // global hash
        std::size_t hash;
    };

    template<typename EnvelParamT, OscillatorType Osc>
    struct SetParam {
      template<typename HarmonicsArray, typename B>
      static void set(int sample_rate, ParamsFor<EnvelParamT, HarmonicsArray, Osc> const & p, B & b) {
        b.forEachElem([&p, sample_rate](auto & e) {
          // the order is important, maybe we need a single method.
          e.getOsc().setHarmonics(p.har, sample_rate);
          e.editEnvelope().setAHDSR(p.env, sample_rate);
        });
      }
    };

    template<typename EnvelParamT>
    struct SetParam<EnvelParamT, OscillatorType::Sweep> {
      template<typename HarmonicsArray, typename B>
      static void set(int sample_rate, ParamsFor<EnvelParamT, HarmonicsArray, OscillatorType::Sweep> const & p, B & b) {
        b.forEachElem([&p, sample_rate](auto & e) {
          // the order is important, maybe we need a single method.
          e.getOsc().setHarmonics(p.har, sample_rate);
          e.editEnvelope().setAHDSR(p.env, sample_rate);
          // Side note : for a sweep, MultiEnveloped is overkill because there is a single harmonic.
          e.getOsc().forEachHarmonic([&p, sample_rate](auto & h) {
            h.getAlgo().getCtrl().setup(p.params.sweep.freq_extremity,
                                        freq_to_angle_increment(p.params.sweep.freq, sample_rate),
                                        p.params.sweep.durationSamples,
                                        p.params.sweep.interp);
          });
        });
      }
    };

    template<Atomicity A, typename T, EnvelopeRelease Rel>
    struct HasNoteOff<AHDSREnvelope<A, T, Rel>> {
      static constexpr bool value = Rel == EnvelopeRelease::WaitForKeyRelease;
    };

    template<typename Env>
    std::pair<std::vector<double>, int>
    envelopeGraphVec(int sample_rate, typename Env::Param const & envParams) {
      Env e;
      e.setAHDSR(envParams, sample_rate);
      // emulate a key-press
      e.onKeyPressed(0);
      int splitAt = -1;

      std::vector<double> v;
      v.reserve(10000);
      for(int i=0; e.getRelaxedState() != EnvelopeState::EnvelopeDone1; ++i) {
        e.step();
        v.push_back(e.value());
        if(!e.afterAttackBeforeSustain()) {
          splitAt = v.size();
          if constexpr (Env::Release == EnvelopeRelease::WaitForKeyRelease) {
            // emulate a key-release
            e.onKeyReleased(0);
          }
          break;
        }
      }
      while(e.getRelaxedState() != EnvelopeState::EnvelopeDone1) {
        e.step();
        v.push_back(e.value());
      }
      return {std::move(v),splitAt};
    }
  }

#ifdef IMJ_AUDIO_MASTERGLOBALLOCK
#pragma message "IMJ_AUDIO_MASTERGLOBALLOCK mode is not recommended, it will lead to audio glitches under contention."
    static constexpr auto audioEnginePolicy = AudioOutPolicy::MasterGlobalLock;
#else
    // This lockfree mode is recommended, it reduces the likelyhood of audio glitches.
    static constexpr auto audioEnginePolicy = AudioOutPolicy::MasterLockFree;
#endif

    static constexpr int nAudioOut = 2;

    using Stepper = SimpleAudioOutContext<
    nAudioOut,
    audioEnginePolicy,
    AudioPostPolicyImpl<nAudioOut, ReverbType::Realtime_Synchronous, audioEnginePolicy>
    >;

    using Ctxt = Context<
      AudioPlatform::PortAudio,
      Features::JustOut,
      Stepper
    >;

    Ctxt & getAudioContext();

    constexpr SynchronizePhase syncPhase(audioelement::OscillatorType o){
      using audioelement::OscillatorType;
      switch(o) {
        case OscillatorType::Saw:
        case OscillatorType::Square:
        case OscillatorType::Triangle:
        case OscillatorType::Sinus:
        case OscillatorType::SinusLoudnessVolumeAdjusted:
          return SynchronizePhase::Yes;

        case OscillatorType::Noise:
        case OscillatorType::Sweep:
          return SynchronizePhase::No;
      }
    }
    constexpr DefaultStartPhase defaultStartPhase(audioelement::OscillatorType o){
      using audioelement::OscillatorType;
      switch(o) {
        case OscillatorType::Saw:
        case OscillatorType::Square:
        case OscillatorType::Triangle:
        case OscillatorType::Sinus:
        case OscillatorType::SinusLoudnessVolumeAdjusted:
          return DefaultStartPhase::Random;

        case OscillatorType::Noise:
        case OscillatorType::Sweep:
          return DefaultStartPhase::Zero;
      }
    }

    template <typename Env, audioelement::OscillatorType Osc>
    using synthOf = sine::Synth < // the name of the namespace is misleading : it can handle all kinds of oscillators
      audioEnginePolicy
    , nAudioOut
    , XfadePolicy::SkipXfade
    , audioelement::audioElementOf<Osc, Env>
    , syncPhase(Osc)
    , defaultStartPhase(Osc)
    , audioelement::HasNoteOff<Env>::value
    , EventIterator>;

    template<typename T>
    struct withChannels {
      withChannels()
      : noteids(150) // max number of simultaneously played pitches on a single synth
      {}

      ~withChannels() {
        std::lock_guard<std::mutex> l(isUsed); // see 'Using'
      }

      auto onEvent(int const sample_rate, Event e, Optional<MIDITimestampAndSource> const & maybeMts) {
        return obj.onEvent(sample_rate,
                           e,
                           getAudioContext().getStepper(),
                           getAudioContext().getStepper(),
                           maybeMts);
      }

      void convertNoteId(Event & e) {
        int const pitch = e.noteid.noteid;
        auto it = noteids.find(pitch);
        switch(e.type) {
          case EventType::NoteOn:
          {
            next.noteid++;
            if (it == noteids.end()) {
              noteids.emplace(pitch, next);
            } else {
              it->second = next;
            }
            e.noteid = next;
            break;
          }
          case EventType::NoteChange:
          {
            if (it == noteids.end()) {
              Assert(0); // a notechange must be preceeded by noteon, and must be before noteoff
              e.noteid.noteid = -pitch;
            } else {
              e.noteid = it->second;
            }
            break;
          }
          case EventType::NoteOff:
          {
            if (it == noteids.end()) {
              Assert(0); // a noteoff must be preceeded by noteon
              e.noteid.noteid = -pitch;
            } else {
              e.noteid = it->second;
              noteids.erase(it);
            }
            break;
          }
        }
      }

      T obj;
      std::mutex isUsed;
    private:
      NoteId next{};
      // for the same pitch, noteon triggers a new noteid, notechange reuses the current noteid for that pitch, noteoff clears that noteid
      std::unordered_map<int/*pitch*/,NoteId> noteids;
    };

    // a 'Using' instance gives the guarantee that the object 'o' passed to its constructor
    // won't be destroyed during the entire lifetime of the instance, iff the following conditions hold:
    //   (1) 'protectsDestruction' passed to the constructor is currently locked
    //   (2) T::~T() locks, then unlocks 'o.isUsed', so that 'o' cannot be destroyed
    //         until 'protectsDestruction' is unlocked
    template<typename T>
    struct Using {
      T & o; // this reference makes the object move-only, which is what we want

      Using(std::lock_guard<std::mutex> && protectsDestruction, T&o) : o(o) {
        o.isUsed.lock();
        // NOTE here, both the instrument lock (isUsed) and the 'protectsDestruction' lock
        // are taken.
        //
        // The order in which we take the locks is important to avoid deadlocks:
        // it is OK to take multiple locks at the same time, /only/ if, everywhere in the program,
        // we take them respecting a global order on the locks of the program.
        //
        // Hence, here the global order is:
        // map lock (protectsDestruction) -> instrument lock (isUsed)
      }
      ~Using() {
        o.isUsed.unlock();
      }
    };

    struct tryScopedLock {
      tryScopedLock(std::mutex&m) : m(m) {
        success = m.try_lock();
      }
      operator bool () const {
        return success;
      }
      ~tryScopedLock() {
        if(success) {
          m.unlock();
        }
      }
    private:
      std::mutex & m;
      bool success;
    };

    template<typename T>
    struct Hash {
      std::size_t operator ()(T const & p) const {
          return p.getHash();
      }
    };

    template <typename Envel, audioelement::OscillatorType Osc>
    struct Synths {
      using T = synthOf<Envel, Osc>;
      using EnvelParamT = typename Envel::Param;

      template<typename HarmonicsArray>
      using Params = audioelement::ParamsFor<EnvelParamT, HarmonicsArray, Osc>;

      // used as key in a map
      using StaticParamsSummary = audioelement::ParamsSummary<EnvelParamT, Osc>;

      // NOTE the 'Using' is constructed while we hold the lock to the map.
      // Hence, while garbage collecting / recycling, if we take the map lock,
      // and if the instrument lock is not taken, we have the guarantee that
      // the instrument lock won't be taken until we release the map lock.
      template<typename HarmonicsArray>
      static Using<withChannels<T>> get(int sample_rate, Params<HarmonicsArray> const &params) {
        using namespace audioelement;
        StaticParamsSummary summary(params);

        // we use a global lock because we can concurrently modify and lookup the map.
        std::lock_guard<std::mutex> l(map_mutex());

        auto & synths = map();
        auto it = synths.find(summary);
        if(it != synths.end()) {
          return Using(std::move(l), *(it->second));
        }
        if(auto * p = recycleInstrument(sample_rate, synths, params, summary)) {
          return Using(std::move(l), *p);
        }
        auto p = std::make_unique<withChannels<T>>();
        SetParam<EnvelParamT, Osc>::set(sample_rate, params, p->obj);
        if(!p->obj.initialize(getAudioContext().getStepper())) {
          auto oneSynth = synths.begin();
          if(oneSynth != synths.end()) {
            LG(ERR, "a preexisting synth is returned");
            return Using(std::move(l), *(oneSynth->second.get()));
          }
          LG(ERR, "an uninitialized synth is returned");
        }
        return Using(
            std::move(l)
          , *(synths.emplace(summary, std::move(p)).first->second));
      }

      static void finalize() {
        std::lock_guard<std::mutex> l(map_mutex());
        for(auto & s : map()) {
          s.second->obj.finalize(getAudioContext().getStepper());
        }
        map().clear();
      }

    private:
      using Map = std::unordered_map<
        StaticParamsSummary,
        std::unique_ptr<withChannels<T>>,
        Hash<StaticParamsSummary>>;

      static auto & map() {
        static Map m;
        return m;
      }
      static auto & map_mutex() {
        static std::mutex m;
        return m;
      }

      /* The caller is expected to take the map mutex. */
      template<typename HarmonicsArray>
      static withChannels<T> * recycleInstrument(int const sample_rate, Map & synths, Params<HarmonicsArray> const & params, StaticParamsSummary const summary) {
        for(auto it = synths.begin(), end = synths.end(); it != end; ++it) {
          auto & i = it->second;
          if(!i) {
            LG(ERR,"inconsistent map");
            continue;
          }
          auto & o = *i;
          if(auto scoped = tryScopedLock(o.isUsed)) {
            if(!o.obj.allEnvelopesFinished()) {
              continue;
            }

            // All enveloppes are finished, and the map mutex has been taken
            // so no note is being started

            /*
            auto node = synths.extract(it);
            node.key() = envelParam;
            auto [inserted, isNew] = synths.insert(std::move(node));
            */
            // the code above uses C++17 features not present in clang yet, it is
            // replaced by the code below.
            std::unique_ptr<withChannels<T>> new_p;
            new_p.swap(it->second);
            synths.erase(it);
            auto [inserted, isNew] = synths.emplace(summary, std::move(new_p));

            Assert(isNew); // because prior to calling this function, we did a lookup
            using namespace audioelement;
            SetParam<EnvelParamT, Osc>::set(sample_rate, params, inserted->second->obj);
            return inserted->second.get();
          }
          else {
            // a note is being started or stopped, we can't recycle this instrument.
          }
        }
        return nullptr;
      }
    };

    template<audioelement::OscillatorType O>
    struct FinalizeSynths {
      void operator ()() {
        using namespace audioelement;
        static constexpr auto A = getAtomicity<audioEnginePolicy>();
        Synths<AHDSREnvelope<A, AudioFloat, EnvelopeRelease::WaitForKeyRelease>, O>::finalize();
        Synths<AHDSREnvelope<A, AudioFloat, EnvelopeRelease::ReleaseAfterDecay>, O>::finalize();
      }
    };

    template<typename Env, audioelement::OscillatorType Osc, typename HarmonicsArray, typename... Args>
    onEventResult midiEvent(HarmonicsArray const & harmonics, typename Env::Param const & env, Event e, Optional<MIDITimestampAndSource> maybeMts, Args... args) {
      audioelement::ParamsFor<typename Env::Param, HarmonicsArray, Osc> params{harmonics, env, std::forward<Args>(args)...};
      std::optional<int> sr = getAudioContext().getSampleRate();
      auto using_synth = Synths<Env, Osc>::get(*sr, params);
      // note id in 'e' is the pitch. we need to convert it:
      using_synth.o.convertNoteId(e);
      return using_synth.o.onEvent(*sr, e, maybeMts);
    }

    template<typename Env, typename HarmonicsArray>
    onEventResult midiEvent_(audioelement::OscillatorType osc, HarmonicsArray const & harmonics, typename Env::Param const & p, Event n, Optional<MIDITimestampAndSource> maybeMts) {
      using namespace audioelement;
      switch(osc) {
        case OscillatorType::Saw:
          return midiEvent<Env, OscillatorType::Saw, HarmonicsArray>(harmonics, p, n, maybeMts);
        case OscillatorType::Square:
          return midiEvent<Env, OscillatorType::Square, HarmonicsArray>(harmonics, p, n, maybeMts);
        case OscillatorType::Triangle:
          return midiEvent<Env, OscillatorType::Triangle, HarmonicsArray>(harmonics, p, n, maybeMts);
        case OscillatorType::Sinus:
          return midiEvent<Env, OscillatorType::Sinus, HarmonicsArray>(harmonics, p, n, maybeMts);
        case OscillatorType::SinusLoudnessVolumeAdjusted:
          return midiEvent<Env, OscillatorType::SinusLoudnessVolumeAdjusted, HarmonicsArray>(harmonics, p, n, maybeMts);
        case OscillatorType::Noise:
          return midiEvent<Env, OscillatorType::Noise, HarmonicsArray>(harmonics, p, n, maybeMts);
        default:
          std::cerr << "not a standard oscillator" << std::endl;
          throw(std::runtime_error("not a standard oscillator"));
      }
    }

    template<typename Env, typename HarmonicsArray>
    onEventResult midiEventSweep_(audioelement::OscillatorType osc,
                                  HarmonicsArray const & harmonics,
                                  typename Env::Param const & p,
                                  audioelement::SweepSetup const & sweep,
                                  Event n,
                                  Optional<MIDITimestampAndSource> maybeMts) {
      using namespace audioelement;
      switch(osc) {
        case OscillatorType::Sweep:
          return midiEvent<Env, OscillatorType::Sweep, HarmonicsArray>(harmonics, p, n, maybeMts, sweep);
        default:
          std::cerr << "not a sweep oscillator" << std::endl;
          throw(std::runtime_error("not a sweep oscillator"));
      }
    }


    using VoiceWindImpl = Voice<audioEnginePolicy, Ctxt::nAudioOut, audioelement::SoundEngineMode::WIND, true>;

    VoiceWindImpl & windVoice();

} // NS imajuscule::audio

#endif
