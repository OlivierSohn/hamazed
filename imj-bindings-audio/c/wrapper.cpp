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

    template<typename T, EnvelopeRelease Rel>
    struct ClampParam<AHDSREnvelope<T, Rel>> {
      static auto clamp(AHDSR_t const & env) {
        return env; // TODO clamp according to AHDSREnvelope
      }
    };

    template<typename T, EnvelopeRelease Rel>
    struct ClampParam<AHPropDerDSREnvelope<T, Rel>> {
      static auto clamp(AHDSR_t const & env) {
        return env; // TODO clamp according to AHPropDerDSREnvelope
      }
    };

    template<typename T>
    struct SetParam<SimpleEnvelope<T>> {
      template<typename A>
      static void set(int envelCharacTime, A & a) {
        a.set_xfade_length(envelCharacTime);
      }
    };

    template<typename T, EnvelopeRelease Rel>
    struct SetParam<AHDSREnvelope<T, Rel>> {
      template<typename A>
      static void set(AHDSR_t const & env, A & a) {
        a.forEachElems([&env](auto & e) { e.algo.editEnveloppe().setAHDSR(env); });
      }
    };

    template<typename T, EnvelopeRelease Rel>
    struct SetParam<AHPropDerDSREnvelope<T, Rel>> {
      template<typename A>
      static void set(AHDSR_t const & env, A & a) {
        a.forEachElems([&env](auto & e) { e.algo.editEnveloppe().setAHDSR(env); });
      }
    };

    template<typename T>
    struct HasNoteOff<SimpleEnvelope<T>> {
      static constexpr bool value = true;
    };

    template<typename T, EnvelopeRelease Rel>
    struct HasNoteOff<AHDSREnvelope<T, Rel>> {
      static constexpr bool value = Rel == EnvelopeRelease::WaitForKeyRelease;
    };

    template<typename T, EnvelopeRelease Rel>
    struct HasNoteOff<AHPropDerDSREnvelope<T, Rel>> {
      static constexpr bool value = Rel == EnvelopeRelease::WaitForKeyRelease;
    };
  }

  namespace audio {

    using AllChans = ChannelsAggregate<
      Channels<2, XfadePolicy::UseXfade, AudioOutPolicy::Master>,
      Channels<2, XfadePolicy::SkipXfade, AudioOutPolicy::Master>
    >;

    using Ctxt = AudioOutContext<
      outputDataBase<
        AudioOutPolicy::Master,
        AllChans
        >,
      Features::JustOut,
      AudioPlatform::PortAudio
      >;

    auto & getAudioContext() {
      static constexpr auto n_max_orchestrator_per_channel = 1;

      static Ctxt c {
          masterAudioLock(),
          std::numeric_limits<uint8_t>::max(),
          n_max_orchestrator_per_channel
        };
      return c;
    }

    auto & getXfadeChannels() {
      return getAudioContext().getChannelHandler().getChannels().getChannels1();
    }
    auto & getNoXfadeChannels() {
      return getAudioContext().getChannelHandler().getChannels().getChannels2();
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

      template <typename Envel>
      struct Synths {
        using T = mySynth::SynthT<Envel>;
        using K = typename Envel::Param;

        static auto & get(K const & rawEnvelParam) {
          using namespace audioelement;

          auto envelParam = ClampParam<Envel>::clamp(rawEnvelParam);
          {
            // we use a global lock because we can concurrently modify and lookup the map.
            imajuscule::scoped::MutexLock l(mutex());

            auto & synths = map();

            auto it = synths.find(envelParam);
            if(it != synths.end()) {
              return *(it->second.get());
            }
            auto unique = std::make_unique<T>();
            SetParam<Envel>::set(envelParam, *unique);
            auto * ret = unique.get();
            if(unique->initialize(getNoXfadeChannels())) {
                auto res = synths.emplace(envelParam, std::move(unique));
                return *(res.first->second.get());
            }
            else {
              LG(ERR,"get().initialize failed");
            }
            auto oneSynth = synths.begin();
            if(oneSynth != synths.end()) {
              LG(ERR, "get : a preexisting synth is returned");
              return *(oneSynth->second.get());
            }
            LG(ERR, "get : an uninitialized synth is returned");
            return *ret;
          }
        }

        static void finalize() {
          using namespace scoped;
          MutexLock l(mutex());
          for(auto & s : map()) {
            s.second->finalize(getNoXfadeChannels());
          }
        }

      private:
        static auto & map() {
          static std::map<K,std::unique_ptr<T>> m;
          return m;
        }
        static auto & mutex() {
          static std::mutex m;
          return m;
        }
      };

      template<typename Env>
      void midiEvent(typename Env::Param const & env, Event e) {
        using namespace mySynth;
        Synths<Env>::get(env).onEvent2(e, getAudioContext().getChannelHandler(), getNoXfadeChannels());
      }

      void midiEventAHDSR(envelType t, AHDSR_t p, Event n) {
        using namespace audioelement;
        switch(t) {
          case AHDSR_ReleaseAfterDecay:
            midiEvent<AHDSREnvelope<float, EnvelopeRelease::ReleaseAfterDecay>>(p, n);
            break;
          case AHDSR_WaitForKeyRelease:
            midiEvent<AHDSREnvelope<float, EnvelopeRelease::WaitForKeyRelease>>(p, n);
            break;
          case AHPropDerDSR_ReleaseAfterDecay:
            midiEvent<AHPropDerDSREnvelope<float, EnvelopeRelease::ReleaseAfterDecay>>(p, n);
            break;
          case AHPropDerDSR_WaitForKeyRelease:
            midiEvent<AHPropDerDSREnvelope<float, EnvelopeRelease::WaitForKeyRelease>>(p, n);
            break;
          default:
            break;
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
    Synths<AHDSREnvelope<float, EnvelopeRelease::WaitForKeyRelease>>::finalize();
    Synths<AHDSREnvelope<float, EnvelopeRelease::ReleaseAfterDecay>>::finalize();
    Synths<AHPropDerDSREnvelope<float, EnvelopeRelease::WaitForKeyRelease>>::finalize();
    Synths<AHPropDerDSREnvelope<float, EnvelopeRelease::ReleaseAfterDecay>>::finalize();

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
