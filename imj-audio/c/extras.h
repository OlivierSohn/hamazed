#include "compiler.prepro.h"
#include "cpp.audio/include/public.h"
#include "memory.h"

#ifdef __cplusplus

extern "C" {
  enum envelType {
      AHDSR_WaitForKeyRelease
    , AHDSR_ReleaseAfterDecay
  };
}

namespace imajuscule {
  namespace audioelement {
    template <typename Envel>
    using VolumeAdjustedOscillator =
      FinalAudioElement<
        Envelopped<
          VolumeAdjusted<
            OscillatorAlgo<
              typename Envel::FPT
            , eNormalizePolicy::FAST
            >
          >
        , Envel
        >
      >;

    template<typename S>
    struct ClampParam;
    template<typename S>
    struct SetParam;
    template<typename S>
    struct HasNoteOff;

    template<Atomicity A, typename T>
    struct ClampParam<SimpleLinearEnvelope<A, T>> {
      static auto clamp(int envelCharacTime) {
        return std::max(envelCharacTime, 100);
      }
    };

    template<Atomicity A, typename T, EnvelopeRelease Rel>
    struct ClampParam<AHDSREnvelope<A, T, Rel>> {
      static auto clamp(AHDSR const & env) {
        return env; // TODO clamp according to AHDSREnvelope
      }
    };

    template<Atomicity A, typename T>
    struct SetParam<SimpleLinearEnvelope<A, T>> {
      template<typename B>
      static void set(int dt, B & b) {
        b.forEachElems([dt](auto & e) { e.algo.editEnveloppe().setEnvelopeCharacTime(dt); });
      }
    };

    template<Atomicity A, typename T, EnvelopeRelease Rel>
    struct SetParam<AHDSREnvelope<A, T, Rel>> {
      template<typename B>
      static void set(AHDSR const & env, B & b) {
        b.forEachElems([&env](auto & e) { e.algo.editEnveloppe().setAHDSR(env); });
      }
    };

    template<Atomicity A, typename T>
    struct HasNoteOff<SimpleLinearEnvelope<A, T>> {
      static constexpr bool value = true;
    };

    template<Atomicity A, typename T, EnvelopeRelease Rel>
    struct HasNoteOff<AHDSREnvelope<A, T, Rel>> {
      static constexpr bool value = Rel == EnvelopeRelease::WaitForKeyRelease;
    };


    template<typename Env>
    std::pair<std::vector<float>, int> envelopeGraphVec(typename Env::Param const & rawEnvParams) {
      Env e;
      auto envParams = ClampParam<Env>::clamp(rawEnvParams);
      e.setAHDSR(envParams);
      // emulate a key-press
      e.onKeyPressed();
      int splitAt = -1;

      std::vector<float> v, v2;
      v.reserve(10000);
      for(int i=0; e.getRelaxedState() != EnvelopeState::EnvelopeDone1; ++i) {
        e.step();
        v.push_back(e.value());
        if(!e.afterAttackBeforeSustain()) {
          splitAt = v.size();
          if constexpr (Env::Release == EnvelopeRelease::WaitForKeyRelease) {
            // emulate a key-release
            e.onKeyReleased();
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

    template<typename Env>
    float* envelopeGraph(typename Env::Param const & rawEnvParams, int*nElems, int*splitAt) {
      std::vector<float> v;
      int split;
      std::tie(v, split) = envelopeGraphVec<Env>(rawEnvParams);
      if(nElems) {
        *nElems = v.size();
      }
      if(splitAt) {
        *splitAt = split;
      }
      auto n_bytes = v.size()*sizeof(decltype(v[0]));
      auto c_arr = imj_c_malloc(n_bytes); // will be freed by haskell finalizer.
      memcpy(c_arr, v.data(), n_bytes);
      return static_cast<float*>(c_arr);
    }

    float* analyzeEnvelopeGraph(envelType t, AHDSR p, int* nElems, int*splitAt);
  }

  namespace audio {

    // The lockfree mode reduces the likelyhood of audio glitches:
    static constexpr auto audioEnginePolicy = AudioOutPolicy::MasterLockFree;
    // However, if you believe using a global lock would fit your use-case better,
    //   use this instead:
    //static constexpr auto audioEnginePolicy = AudioOutPolicy::MasterGlobalLock;

    using AllChans = ChannelsVecAggregate< 2, audioEnginePolicy >;

    using NoXFadeChans = typename AllChans::NoXFadeChans;
    using XFadeChans = typename AllChans::XFadeChans;

    using ChannelHandler = outputDataBase< AllChans >;

    using Ctxt = AudioOutContext<
      ChannelHandler,
      Features::JustOut,
      AudioPlatform::PortAudio
      >;

    Ctxt & getAudioContext();

    XFadeChans *& getXfadeChannels();

    Event mkNoteOn(int pitch, float velocity);

    Event mkNoteOff(int pitch);

    // no error above this

    namespace sine {
      template <typename Env>
      using SynthT = Synth <
        Ctxt::policy
      , Ctxt::nAudioOut
      , XfadePolicy::SkipXfade
      , audioelement::Oscillator<Env>
      , audioelement::HasNoteOff<Env>::value
      , EventIterator<IEventList>
      , NoteOnEvent
      , NoteOffEvent>;
    }

    namespace vasine {
      template <typename Env>
      using SynthT = Synth <
        Ctxt::policy
      , Ctxt::nAudioOut
      , XfadePolicy::SkipXfade
      , audioelement::VolumeAdjustedOscillator<Env>
      , audioelement::HasNoteOff<Env>::value
      , EventIterator<IEventList>
      , NoteOnEvent
      , NoteOffEvent>;
    }
    namespace mySynth = imajuscule::audio::vasine;
    //namespace mySynth = imajuscule::audio::sine;

    template<typename T>
    struct withChannels {
      withChannels(NoXFadeChans & chans) : chans(chans), obj(buffers) {}
      ~withChannels() {
        std::lock_guard<std::mutex> l(isUsed); // see 'Using'
      }

      template<typename Out>
      void onEvent2(Event e, Out & out) {
        obj.onEvent2(e, out, chans);
      }

      void finalize() {
        obj.finalize();
      }

      T obj;

      // Note that if mononotechannel used pointers to the channels instead of ids
      // we would not need this field:
      NoXFadeChans & chans;

      std::mutex isUsed;

      static constexpr auto n_mnc = T::n_channels;
      using mnc_buffer = typename T::MonoNoteChannel::buffer_t;
      std::array<mnc_buffer,n_mnc> buffers;
    };

    // a 'Using' instance gives the guarantee that the object 'o' passed to its constructor
    // won't be destroyed during the entire lifetime of the instance, iff the following conditions hold:
    //   (1) T::~T() locks, then unlocks 'o.isUsed'
    //   (2) 'protectsDestruction' passed to the constructor is currently locked
    //          and 'o' cannot be destroyed until 'protectsDestruction' is unlocked
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

    template <typename Envel>
    struct Synths {
      using T = mySynth::SynthT<Envel>;
      using K = typename Envel::Param;

      // NOTE the 'Using' is constructed while we hold the lock to the map.
      // Hence, while garbage collecting / recycling, if we take the map lock,
      // and if the instrument lock is not taken, we have the guarantee that
      // the instrument lock won't be taken until we release the map lock.
      static Using<withChannels<T>> get(K const & rawEnvelParam) {
        using namespace audioelement;

        auto envelParam = ClampParam<Envel>::clamp(rawEnvelParam);
        {
          // we use a global lock because we can concurrently modify and lookup the map.
          std::lock_guard<std::mutex> l(mutex());

          auto & synths = map();

          auto it = synths.find(envelParam);
          if(it != synths.end()) {
            return Using(std::move(l), *(it->second));
          }
          if(auto * p = recycleInstrument(synths, envelParam)) {
            return Using(std::move(l), *p);
          }
          auto [c,remover] = addNoXfadeChannels(T::n_channels);
          auto p = std::make_unique<withChannels<T>>(c);
          SetParam<Envel>::set(envelParam, p->obj);
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
            , *(synths.emplace(envelParam, std::move(p)).first->second));
        }
      }

      static void finalize() {
        std::lock_guard<std::mutex> l(mutex());
        for(auto & s : map()) {
          s.second->finalize();
        }
        map().clear();
      }

    private:
      using Map = std::map<K,std::unique_ptr<withChannels<T>>>;

      static auto & map() {
        static Map m;
        return m;
      }
      static auto & mutex() {
        static std::mutex m;
        return m;
      }

      /* The caller is expected to take the map mutex. */
      static withChannels<T> * recycleInstrument(Map & synths, K const & envelParam) {
        for(auto it = synths.begin(), end = synths.end(); it != end; ++it) {
          auto & i = it->second;
          if(!i) {
            LG(ERR,"inconsistent map");
            continue;
          }
          auto & o = *i;
          if(auto scoped = tryScopedLock(o.isUsed)) {
            // we don't take the audio lock because hasOrchestratorsOrComputes relies on an
            // atomically incremented / decremented counter.
            if(o.chans.hasOrchestratorsOrComputes()) {
              continue;
            }

            // - we have 0 orchestrator and 0 computes
            // - no note is being started, hence we won't increase the number of orchestrators or computes.
            // Hence, all enveloppes should be finished : if one is not finished, it will not have a chance to ever finish.
            Assert(o.obj.areEnvelopesFinished() && "inconsistent envelopes");

            // this code uses c++17 features not present in clang yet, so it's replaced by the code after.
            /*
            auto node = synths.extract(it);
            node.key() = envelParam;
            auto [inserted, isNew] = synths.insert(std::move(node));
            */
            std::unique_ptr<withChannels<T>> new_p;
            new_p.swap(it->second);
            synths.erase(it);
            auto [inserted, isNew] = synths.emplace(envelParam, std::move(new_p));

            Assert(isNew); // because prior to calling this function, we did a lookup
            using namespace audioelement;
            SetParam<Envel>::set(envelParam, inserted->second->obj);
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

    template<typename Env>
    void midiEvent(typename Env::Param const & env, Event e) {
      Synths<Env>::get(env).o.onEvent2(e, getAudioContext().getChannelHandler());
    }

    using VoiceWindImpl = Voice<Ctxt::policy, Ctxt::nAudioOut, audio::SoundEngineMode::WIND, true>;

    VoiceWindImpl & windVoice();

  } // NS audio

  namespace audioelement {

      void midiEventAHDSR(envelType t, AHDSR p, audio::Event n);

  } // NS audioelement

}

#endif
