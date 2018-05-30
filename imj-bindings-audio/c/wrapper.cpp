#ifdef __cplusplus

#include "cpp.audio/include/public.h"

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

    template<typename T>
    struct ClampParam<SimpleEnvelope<T>> {
      static auto clamp(int envelCharacTime) {
        return std::max(envelCharacTime, 100);
      }
    };

    template<typename T, DecayInterpolation DecayItp, EnvelopeRelease Rel>
    struct ClampParam<AHDSREnvelope<T, DecayItp, Rel>> {
      static auto clamp(AHDSR_t const & env) {
        return env; // TODO clamp according to AHDSREnvelope
      }
    };

    template<typename T>
    struct SetParam<SimpleEnvelope<T>> {
      template<typename A>
      static void set(int envelCharacTime, A & a) {
        a.set_xfade_length(envelCharacTime);
      }
    };

    template<typename T, DecayInterpolation DecayItp, EnvelopeRelease Rel>
    struct SetParam<AHDSREnvelope<T, DecayItp, Rel>> {
      template<typename A>
      static void set(AHDSR_t const & env, A & a) {
        a.forEachElems([&env](auto & e) { e.algo.editEnveloppe().setAHDSR(env); });
      }
    };

    template<typename T>
    struct HasNoteOff<SimpleEnvelope<T>> {
      static constexpr bool value = true;
    };

    template<typename T, DecayInterpolation DecayItp, EnvelopeRelease Rel>
    struct HasNoteOff<AHDSREnvelope<T, DecayItp, Rel>> {
      static constexpr bool value = Rel == EnvelopeRelease::WaitForKeyRelease;
    };
  }

  namespace audio {

    using AllChans = ChannelsVecAggregate< 2, AudioOutPolicy::Master >;

    using NoXFadeChans = typename AllChans::NoXFadeChans;
    using XFadeChans = typename AllChans::XFadeChans;

    using Ctxt = AudioOutContext<
      outputDataBase<
        AudioOutPolicy::Master,
        AllChans
        >,
      Features::JustOut,
      AudioPlatform::PortAudio
      >;

    auto & getAudioContext() {
      static Ctxt c { GlobalAudioLock<AudioOutPolicy::Master>::get() };
      return c;
    }

    auto & getXfadeChannels() {
      return **(getAudioContext().getChannelHandler().getChannels().getChannelsXFade().begin());
    }

    auto & addNoXfadeChannels(int nVoices) {
      static constexpr auto n_max_orchestrator_per_channel = 0; // we don't use orchestrators
      auto p = std::make_unique<NoXFadeChans>(
        getAudioContext().getChannelHandler().get_lock_policy(),
        std::min(nVoices, static_cast<int>(std::numeric_limits<uint8_t>::max())),
        n_max_orchestrator_per_channel);
      auto & res = *p;
      {
        typename Ctxt::Locking l(getAudioContext().getChannelHandler().get_lock());
        getAudioContext().getChannelHandler().getChannels().getChannelsNoXFade().emplace_back(std::move(p));
      }
      return res;
    }

    Event mkNoteOn(int pitch, float velocity) {
      Event e;
      e.type = Event::kNoteOnEvent;
      e.noteOn.pitch = pitch;
      e.noteOn.velocity = velocity;
      e.noteOn.channel=0; // unused
      e.noteOn.tuning = 0;
      e.noteOn.noteId = -1;
      e.noteOn.length = std::numeric_limits<decltype(e.noteOn.length)>::max();
      return e;
    }

    Event mkNoteOff(int pitch) {
      Event e;
      e.type = Event::kNoteOffEvent;
      e.noteOff.pitch = pitch;
      e.noteOff.velocity = 0.f;
      e.noteOff.channel= 0;
      e.noteOff.tuning = 0;
      e.noteOff.noteId = -1;
      return e;
    }

    auto & windVoice () {
      constexpr auto n_audio_out = 2;
      constexpr bool withNoteOff = true;
      using VoiceWindImpl = Voice<n_audio_out, audio::SoundEngineMode::WIND, withNoteOff>;
      static VoiceWindImpl v;
      return v;
    }

    namespace sine {
      template <typename Env>
      using SynthT = Synth <
        Ctxt::nAudioOut
      , XfadePolicy::SkipXfade
      , MonoNoteChannel<audioelement::Oscillator<Env>>
      , audioelement::HasNoteOff<Env>::value
      , EventIterator<IEventList>
      , NoteOnEvent
      , NoteOffEvent>;
    }

    namespace vasine {
      template <typename Env>
      using SynthT = Synth <
        Ctxt::nAudioOut
      , XfadePolicy::SkipXfade
      , MonoNoteChannel<audioelement::VolumeAdjustedOscillator<Env>>
      , audioelement::HasNoteOff<Env>::value
      , EventIterator<IEventList>
      , NoteOnEvent
      , NoteOffEvent>;
    }
  }
}

extern "C" {
  enum envelType {
      AHDSR_WaitForKeyRelease
    , AHDSR_ReleaseAfterDecay
    , AHPropDerDSR_WaitForKeyRelease
    , AHPropDerDSR_ReleaseAfterDecay
  };
}

// functions herein are /not/ part of the interface
namespace imajuscule {
  namespace audio {
    namespace detail {

      namespace mySynth = imajuscule::audio::vasine;
      //namespace mySynth = imajuscule::audio::sine;

      // this is very temporary, until mononotechannel uses pointers to the channels instead of ids.
      template<typename T>
      struct withChannels {
        withChannels(NoXFadeChans & chans) : chans(chans) {}
        ~withChannels() {
          std::lock_guard<std::mutex> l(isUsed); // see 'Using'
        }

        template<typename Out>
        void onEvent2(Event e, Out & out) {
          obj.onEvent2(e, out, chans);
        }

        void finalize() {
          obj.finalize(chans);
        }

        T obj;
        NoXFadeChans & chans;
        std::mutex isUsed;
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

      template <typename Envel>
      struct Synths {
        using T = mySynth::SynthT<Envel>;
        using K = typename Envel::Param;

        // NOTE the 'Using' is constructed while we hold the lock to the map.
        // Hence, while garbage collecting, if we take the map lock,
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
            auto p = std::make_unique<withChannels<T>>(addNoXfadeChannels(T::n_channels));
            auto & res = *p;
            SetParam<Envel>::set(envelParam, p->obj);
            if(p->obj.initialize(p->chans)) {
                auto res = synths.emplace(envelParam, std::move(p));
                return Using(std::move(l), *(res.first->second));
            }
            else {
              LG(ERR,"get().initialize failed");
            }
            auto oneSynth = synths.begin();
            if(oneSynth != synths.end()) {
              LG(ERR, "get : a preexisting synth is returned");
              return Using(std::move(l), *(oneSynth->second.get()));
            }
            LG(ERR, "get : an uninitialized synth is returned");
            return Using(std::move(l), res);
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
        static auto & map() {
          static std::map<K,std::unique_ptr<withChannels<T>>> m;
          return m;
        }
        static auto & mutex() {
          static std::mutex m;
          return m;
        }
      };

      template<typename Env>
      void midiEvent(typename Env::Param const & env, Event e) {
        // if we garbage collect std::unique_ptr<withChannels<T>>,
        // we should take the map lock, and we should have a lock in withInstrument
        // indicating that we're issuing a command using this instrument.
        // Here, the instrument lock should be taken _while_ the map lock is taken
        // and released after onEvent2 returns.
        Synths<Env>::get(env).o.onEvent2(e, getAudioContext().getChannelHandler());
      }

      template<typename Env>
      std::pair<std::vector<float>, int> envelopeGraphVec(typename Env::Param const & rawEnvParams) {
        Env e;
        using namespace audioelement;
        auto envParams = ClampParam<Env>::clamp(rawEnvParams);
        e.setAHDSR(envParams);
        // emulate a key-press
        e.onKeyPressed();
        int splitAt = -1;

        std::vector<float> v, v2;
        v.reserve(10000);
        for(int i=0; e.getState() != EnvelopeState::EnvelopeDone1; ++i) {
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
        while(e.getState() != EnvelopeState::EnvelopeDone1) {
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
        auto c_arr = malloc(n_bytes); // will be freed by haskell finalizer.
        memcpy(c_arr, v.data(), n_bytes);
        return static_cast<float*>(c_arr);
      }

      void midiEventAHDSR(envelType t, AHDSR_t p, Event n) {
        using namespace audioelement;
        switch(t) {
          case AHDSR_ReleaseAfterDecay:
            midiEvent<AHDSREnvelope<float, DecayInterpolation::DecayLinear, EnvelopeRelease::ReleaseAfterDecay>>(p, n);
            break;
          case AHDSR_WaitForKeyRelease:
            midiEvent<AHDSREnvelope<float, DecayInterpolation::DecayLinear, EnvelopeRelease::WaitForKeyRelease>>(p, n);
            break;
          case AHPropDerDSR_ReleaseAfterDecay:
            midiEvent<AHDSREnvelope<float, DecayInterpolation::DecayProportionalDerivative, EnvelopeRelease::ReleaseAfterDecay>>(p, n);
            break;
          case AHPropDerDSR_WaitForKeyRelease:
            midiEvent<AHDSREnvelope<float, DecayInterpolation::DecayProportionalDerivative, EnvelopeRelease::WaitForKeyRelease>>(p, n);
            break;
          default:
            break;
        }
      }

      float* analyzeEnvelopeGraph(envelType t, AHDSR_t p, int* nElems, int*splitAt) {
        using namespace audioelement;
        switch(t) {
          case AHDSR_ReleaseAfterDecay:
            return envelopeGraph<AHDSREnvelope<float, DecayInterpolation::DecayLinear, EnvelopeRelease::ReleaseAfterDecay>>(p, nElems, splitAt);
          case AHDSR_WaitForKeyRelease:
            return envelopeGraph<AHDSREnvelope<float, DecayInterpolation::DecayLinear, EnvelopeRelease::WaitForKeyRelease>>(p, nElems, splitAt);
          case AHPropDerDSR_ReleaseAfterDecay:
            return envelopeGraph<AHDSREnvelope<float, DecayInterpolation::DecayProportionalDerivative, EnvelopeRelease::ReleaseAfterDecay>>(p, nElems, splitAt);
          case AHPropDerDSR_WaitForKeyRelease:
            return envelopeGraph<AHDSREnvelope<float, DecayInterpolation::DecayProportionalDerivative, EnvelopeRelease::WaitForKeyRelease>>(p, nElems, splitAt);
          default:
            return {};
        }
      }
    }
  }
}

// functions herein are part of the interface
extern "C" {

  bool initializeAudio () {
    using namespace std;
    using namespace imajuscule;
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::detail;
#ifndef NDEBUG
    cout << "WARNING : C++ sources of imj-bindings-audio were built without NDEBUG" << endl;
#endif

    disableDenormals();

    setPortaudioEnvVars();

    getAudioContext().Init();

    // add a single Xfade channel (needed because soundengine and channel don't support envelopes entirely)
    static constexpr auto n_max_orchestrator_per_channel = 1;
    getAudioContext().getChannelHandler().getChannels().getChannelsXFade().emplace_back(
      std::make_unique<XFadeChans>(
        getAudioContext().getChannelHandler().get_lock_policy(),
        std::numeric_limits<uint8_t>::max(),
        n_max_orchestrator_per_channel));

    windVoice().initializeSlow();
    if(!windVoice().initialize(getXfadeChannels())) {
      LG(ERR,"windVoice().initialize failed");
      return false;
    }
    return true;
  }

  void stopAudioGracefully() {
    using namespace imajuscule::audio;
    getAudioContext().onApplicationShouldClose();
  }

  void teardownAudio() {
    using namespace imajuscule;
    using namespace imajuscule::audio;
    using namespace imajuscule::audioelement;
    using namespace imajuscule::audio::detail;

    windVoice().finalize(getXfadeChannels());

    Synths<SimpleEnvelope<float>>::finalize();
    Synths<AHDSREnvelope<float, DecayInterpolation::DecayLinear, EnvelopeRelease::WaitForKeyRelease>>::finalize();
    Synths<AHDSREnvelope<float, DecayInterpolation::DecayLinear, EnvelopeRelease::ReleaseAfterDecay>>::finalize();
    Synths<AHDSREnvelope<float, DecayInterpolation::DecayProportionalDerivative, EnvelopeRelease::WaitForKeyRelease>>::finalize();
    Synths<AHDSREnvelope<float, DecayInterpolation::DecayProportionalDerivative, EnvelopeRelease::ReleaseAfterDecay>>::finalize();

    getAudioContext().TearDown();
  }

  void midiNoteOn(int envelCharacTime, int16_t pitch, float velocity) {
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::detail;
    using namespace imajuscule::audioelement;
    midiEvent<SimpleEnvelope<float>>(envelCharacTime, mkNoteOn(pitch,velocity));
  }
  void midiNoteOff(int envelCharacTime, int16_t pitch) {
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::detail;
    using namespace imajuscule::audioelement;
    midiEvent<SimpleEnvelope<float>>(envelCharacTime, mkNoteOff(pitch));
  }

  void midiNoteOnAHDSR_(envelType t, int a, int h, int d, float s, int r, int16_t pitch, float velocity) {
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::detail;
    auto p = AHDSR_t{a,h,d,r,s};
    auto n = mkNoteOn(pitch,velocity);
    midiEventAHDSR(t, p, n);
  }
  void midiNoteOffAHDSR_(envelType t, int a, int h, int d, float s, int r, int16_t pitch) {
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::detail;
    auto p = AHDSR_t{a,h,d,r,s};
    auto n = mkNoteOff(pitch);
    midiEventAHDSR(t, p, n);
  }

  float* analyzeAHDSREnvelope_(envelType t, int a, int h, int d, float s, int r, int*nElems, int*splitAt) {
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::detail;
    auto p = AHDSR_t{a,h,d,r,s};
    return analyzeEnvelopeGraph(t, p, nElems, splitAt);
  }

  void effectOn(int program, int16_t pitch, float velocity) {
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::detail;
    auto voicing = Voicing(program,pitch,velocity,0.f,true,0);
    playOneThing(windVoice(),getAudioContext().getChannelHandler(),getXfadeChannels(),voicing);
  }
  void effectOff(int16_t pitch) {
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::detail;
    stopPlaying(windVoice(),getAudioContext().getChannelHandler(),getXfadeChannels(),pitch);
  }
}

#endif
