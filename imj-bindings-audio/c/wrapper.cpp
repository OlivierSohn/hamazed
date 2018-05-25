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
      using AudioOutSynth = Synth <
        Ctxt::nAudioOut
      , XfadePolicy::SkipXfade // note that this matters only when the VST wrapper is used.
                               // in our case, we're bound to the policy of outputData.
                               // TODO do not depend on AudioOut directly, depend on a new class holding outputData
                               // so that we can use a OutputData with other xfade settings.
      , MonoNoteChannel<audioelement::Oscillator<audioelement::SimpleEnvelope<float>>>
      , true
      , EventIterator<IEventList>
      , NoteOnEvent
      , NoteOffEvent>;
    }

    namespace vasine {
      using AudioOutSynth = Synth <
        Ctxt::nAudioOut
      , XfadePolicy::SkipXfade // note that this matters only when the VST wrapper is used.
                               // in our case, we're bound to the policy of outputData.
                               // TODO do not depend on AudioOut directly, depend on a new class holding outputData
                               // so that we can use a OutputData with other xfade settings.
      , MonoNoteChannel<audioelement::VolumeAdjustedOscillator<audioelement::SimpleEnvelope<float>>>
      , true
      , EventIterator<IEventList>
      , NoteOnEvent
      , NoteOffEvent>;
    }
  }
}

// functions herein are /not/ part of the interface
namespace imajuscule {
  namespace audio {
    namespace detail {

      namespace mySynth = imajuscule::audio::vasine;
      //namespace mySynth = imajuscule::audio::sine;

      auto & allSynths() {
        static std::map<int,std::unique_ptr<mySynth::AudioOutSynth>> sByEnvelCharacTime;
        return sByEnvelCharacTime;
      }

      auto & synthsMutex() {
          static std::mutex m;
          return m;
      }

      mySynth::AudioOutSynth & getSynth(int envelCharacTime)
      {
        auto value = std::max(envelCharacTime, 100);
        {
          // we use a global lock because we can concurrently modify and lookup the map.
          imajuscule::scoped::MutexLock l(synthsMutex());

          auto it = allSynths().find(value);
          if(it != allSynths().end()) {
            return *(it->second.get());
          }
          auto unique = std::make_unique<mySynth::AudioOutSynth>();
          unique->set_xfade_length(value);
          auto * ret = unique.get();
          if(unique->initialize(getNoXfadeChannels())) {
              auto res = allSynths().emplace(value, std::move(unique));
              return *(res.first->second.get());
          }
          else {
            LG(ERR,"getSynth().initialize failed");
          }
          auto oneSynth = allSynths().begin();
          if(oneSynth != allSynths().end()) {
            LG(ERR, "getSynth : a preexisting synth is returned");
            return *(oneSynth->second.get());
          }
          LG(ERR, "getSynth : an uninitialized synth is returned");
          return *ret;
        }
      }

      void midiEvent(int envelCharacTime, Event e) {
        using namespace imajuscule::audio;
        using namespace mySynth;
        getSynth(envelCharacTime).onEvent2(e, getAudioContext().getChannelHandler(), getNoXfadeChannels());
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
    using namespace imajuscule::audio::detail;
    using namespace imajuscule::scoped;
    MutexLock l(synthsMutex());

    windVoice().finalize(getXfadeChannels());
    for(auto & s : allSynths()) {
      s.second->finalize(getNoXfadeChannels());
    }

    getAudioContext().TearDown();
  }

  void midiNoteOn(int envelCharacTime, int16_t pitch, float velocity) {
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::detail;
    midiEvent(envelCharacTime, mkNoteOn(pitch,velocity));
  }
  void midiNoteOff(int envelCharacTime, int16_t pitch) {
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::detail;
    midiEvent(envelCharacTime, mkNoteOff(pitch));
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
