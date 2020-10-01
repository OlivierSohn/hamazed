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
            LoudnessVolumeAdjusted< OscillatorAlgo< FPT, eNormalizePolicy::FAST > >,
            std::conditional_t<
              O == OscillatorType::Sinus,
                OscillatorAlgo< FPT, eNormalizePolicy::FAST >,
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
        FinalAudioElement<
          VolumeAdjusted<
            MultiEnveloped<
              genericOscillator<O, typename Env::FPT>
            , Env
            >
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
      static void set(ParamsFor<EnvelParamT, HarmonicsArray, Osc> const & p, B & b) {
        b.forEachElems([&p](auto & e) {
          // the order is important, maybe we need a single method.
          e.algo.getOsc().setHarmonics(p.har);
          e.algo.editEnvelope().setAHDSR(p.env, SAMPLE_RATE);
        });
      }
    };

    template<typename EnvelParamT>
    struct SetParam<EnvelParamT, OscillatorType::Sweep> {
      template<typename HarmonicsArray, typename B>
      static void set(ParamsFor<EnvelParamT, HarmonicsArray, OscillatorType::Sweep> const & p, B & b) {
        b.forEachElems([&p](auto & e) {
          // the order is important, maybe we need a single method.
          e.algo.getOsc().setHarmonics(p.har);
          e.algo.editEnvelope().setAHDSR(p.env, SAMPLE_RATE);
          // Side note : for a sweep, MultiEnveloped is overkill because there is a single harmonic.
          e.algo.getOsc().forEachHarmonic([&p](auto & h) {
            h.getAlgo().getCtrl().setup(p.params.sweep.freq_extremity,
                                        freq_to_angle_increment(p.params.sweep.freq),
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
    std::pair<std::vector<double>, int> envelopeGraphVec(typename Env::Param const & envParams) {
      Env e;
      e.setAHDSR(envParams, SAMPLE_RATE);
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

    using AllChans = ChannelsVecAggregate< 2, audioEnginePolicy >;

    using NoXFadeChans = typename AllChans::NoXFadeChans;
    using XFadeChans = typename AllChans::XFadeChans;

    using ChannelHandler = outputDataBase< AllChans, ReverbType::Realtime_Synchronous >;

    using Ctxt = AudioOutContext<
      ChannelHandler,
      Features::JustOut,
      AudioPlatform::PortAudio
      >;

    Ctxt & getAudioContext();

    NoXFadeChans *& getNoXfadeChannels();

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
      Ctxt::policy
    , Ctxt::nAudioOut
    , XfadePolicy::SkipXfade
    , audioelement::audioElementOf<Osc, Env>
    , syncPhase(Osc)
    , defaultStartPhase(Osc)
    , audioelement::HasNoteOff<Env>::value
    , EventIterator>;

    template<typename T>
    struct withChannels {
      withChannels(NoXFadeChans & chans) : chans(chans), obj(SAMPLE_RATE, buffers) {}
      ~withChannels() {
        std::lock_guard<std::mutex> l(isUsed); // see 'Using'
      }

      template<typename Out>
      auto onEvent2(Event e, Out & out, Optional<MIDITimestampAndSource> maybeMts) {
        return obj.onEvent2(e, out, chans, maybeMts);
      }

      void finalize() {
        obj.finalize();
      }

      T obj;
      NoXFadeChans & chans;
      std::mutex isUsed;

      static constexpr auto n_mnc = T::n_channels;
      using mnc_buffer = typename T::MonoNoteChannel::buffer_t;
      std::array<mnc_buffer,n_mnc> buffers;
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
      static Using<withChannels<T>> get(Params<HarmonicsArray> const &params) {
        using namespace audioelement;
        StaticParamsSummary summary(params);

        // we use a global lock because we can concurrently modify and lookup the map.
        std::lock_guard<std::mutex> l(map_mutex());

        auto & synths = map();
        auto it = synths.find(summary);
        if(it != synths.end()) {
          return Using(std::move(l), *(it->second));
        }
        if(auto * p = recycleInstrument(synths, params, summary)) {
          return Using(std::move(l), *p);
        }
        auto [c,remover] = addNoXfadeChannels(T::n_channels);
        auto p = std::make_unique<withChannels<T>>(c);
        SetParam<EnvelParamT, Osc>::set(params, p->obj);
        if(!p->obj.initialize(p->chans)) {
          auto oneSynth = synths.begin();
          if(oneSynth != synths.end()) {
            LG(ERR, "a preexisting synth is returned");
            // The channels have the same lifecycle as the instrument, the instrument will be destroyed
            //  so we remove the associated channels:
            remover.flagForRemoval();
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
          s.second->finalize();
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
      static withChannels<T> * recycleInstrument(Map & synths, Params<HarmonicsArray> const & params, StaticParamsSummary const summary) {
        for(auto it = synths.begin(), end = synths.end(); it != end; ++it) {
          auto & i = it->second;
          if(!i) {
            LG(ERR,"inconsistent map");
            continue;
          }
          auto & o = *i;
          if(auto scoped = tryScopedLock(o.isUsed)) {
            // we don't take the audio lock because 'hasRealtimeFunctions' relies on an
            // atomically incremented / decremented counter.
            if(o.chans.hasRealtimeFunctions()) {
              continue;
            }

            // We can assume that all enveloppes are finished : should one
            // not be finished, it would not have a chance to ever finish
            // because there is 0 real-time std::function (oneShots/orchestrator/compute),
            // and no note is being started, because the map mutex has been taken.
            Assert(o.obj.areEnvelopesFinished() && "inconsistent envelopes");

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
            SetParam<EnvelParamT, Osc>::set(params, inserted->second->obj);
            return inserted->second.get();
          }
          else {
            // a note is being started or stopped, we can't recycle this instrument.
          }
        }
        return nullptr;
      }

      static auto addNoXfadeChannels(int nVoices) {
        static constexpr auto n_max_orchestrator_per_channel = 0; // we don't use orchestrators
        return getAudioContext().getChannelHandler().getChannels().getChannelsNoXFade().emplace_front(
          getAudioContext().getChannelHandler().get_lock_policy(),
          std::min(nVoices, static_cast<int>(std::numeric_limits<uint8_t>::max())),
          n_max_orchestrator_per_channel);
      }
    };

    template<audioelement::OscillatorType O>
    struct FinalizeSynths {
      void operator ()() {
        using namespace audioelement;
        static constexpr auto A = getAtomicity<audio::Ctxt::policy>();
        Synths<AHDSREnvelope<A, AudioFloat, EnvelopeRelease::WaitForKeyRelease>, O>::finalize();
        Synths<AHDSREnvelope<A, AudioFloat, EnvelopeRelease::ReleaseAfterDecay>, O>::finalize();
      }
    };

    template<typename Env, audioelement::OscillatorType Osc, typename HarmonicsArray, typename... Args>
    onEventResult midiEvent(HarmonicsArray const & harmonics, typename Env::Param const & env, Event e, Optional<MIDITimestampAndSource> maybeMts, Args... args) {
      audioelement::ParamsFor<typename Env::Param, HarmonicsArray, Osc> params{harmonics, env, std::forward<Args>(args)...};
      return Synths<Env, Osc>::get(params).o.onEvent2(e, getAudioContext().getChannelHandler(), maybeMts);
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


    using VoiceWindImpl = Voice<Ctxt::policy, Ctxt::nAudioOut, audioelement::SoundEngineMode::WIND, true>;

    VoiceWindImpl & windVoice();

} // NS imajuscule::audio

#endif
