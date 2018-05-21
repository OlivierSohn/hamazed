#ifdef __cplusplus

#include <iostream>

#if __APPLE__
#include <fenv.h>
#endif

#include "cpp.os.audio/include/public.h"

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
        AudioOut::nAudioOut
      , XfadePolicy::SkipXfade // note that this matters only when the VST wrapper is used.
                               // in our case, we're bound to the policy of outputData.
                               // TODO do not depend on AudioOut directly, depend on a new class holding outputData
                               // so that we can use a OutputData with other xfade settings.
      , MonoNoteChannel<audioelement::Oscillator<audioelement::SimpleEnveloppe<float>>>
      , true
      , EventIterator<IEventList>
      , NoteOnEvent
      , NoteOffEvent>;
    }

    namespace vasine {
      using AudioOutSynth = Synth <
        AudioOut::nAudioOut
      , XfadePolicy::SkipXfade // note that this matters only when the VST wrapper is used.
                               // in our case, we're bound to the policy of outputData.
                               // TODO do not depend on AudioOut directly, depend on a new class holding outputData
                               // so that we can use a OutputData with other xfade settings.
      , MonoNoteChannel<audioelement::VolumeAdjustedOscillator<audioelement::SimpleEnveloppe<float>>>
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

      mySynth::AudioOutSynth & getSynth() {
        static mySynth::AudioOutSynth s;
        return s;
      }

      void midiEvent(Event e) {
        using namespace imajuscule::audio;
        using namespace mySynth;
        if(auto a = Audio::getInstance()) {
          getSynth().onEvent2(e, a->out().getChannelHandler());
        }
      }

      /*
      * Denormals can appear in reverb algorithm, when signal becomes close to 0.
      */
      void disableDenormals() {
        #if __APPLE__
            fesetenv(FE_DFL_DISABLE_SSE_DENORMS_ENV);
        #else
        #define CSR_FLUSH_TO_ZERO         (1 << 15)
            unsigned csr = __builtin_ia32_stmxcsr();
            csr |= CSR_FLUSH_TO_ZERO;
            __builtin_ia32_ldmxcsr(csr);
        #endif
      }
    }
  }
}

// functions herein are part of the interface
extern "C" {

  bool initializeAudio () {
    using namespace std;
    using namespace imajuscule;
    using namespace imajuscule::audio::detail;
#ifndef NDEBUG
    cout << "WARNING : C++ sources of imj-bindings-audio were built without NDEBUG" << endl;
#endif

    disableDenormals();

    // We know we're going to use audio so we force initialization.
    Audio::Init(Audio::OutInitPolicy::FORCE);
    using namespace imajuscule::audio;

    windVoice().initializeSlow();
    if(auto a = Audio::getInstance()) {
      auto & s = getSynth();
      if(!s.initialize(a->out().getChannelHandler())) {
        LG(ERR,"getSynth().initialize failed");
        return false;
      }
      s.set_xfade_length(10000);

      if(!windVoice().initialize(a->out().getChannelHandler())) {
        LG(ERR,"windVoice().initialize failed");
        return false;
      }
    }
    return true;
  }

  void stopAudioGracefully() {
    if(auto a = Audio::getInstance()) {
      a->out().onApplicationShouldClose();
    }
  }

  void teardownAudio() {
    using namespace imajuscule;
    Audio::TearDown();
  }

  void midiNoteOn(int16_t pitch, float velocity) {
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::detail;
    midiEvent(mkNoteOn(pitch,velocity));
  }
  void midiNoteOff(int16_t pitch) {
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::detail;
    midiEvent(mkNoteOff(pitch));
  }

  void effectOn(int program, int16_t pitch, float velocity) {
    using namespace imajuscule::audio;
    if(auto a = Audio::getInstance()) {
      auto voicing = Voicing(program,pitch,velocity,0.f,true,0);
      playOneThing(windVoice(),a->out().getChannelHandler(),voicing);
    }
  }
  void effectOff(int16_t pitch) {
    using namespace imajuscule::audio;
    if(auto a = Audio::getInstance()) {
      stopPlaying(windVoice(),a->out().getChannelHandler(),pitch);
    }
  }

  // Test function to verify that audio can be played : you should hear a beep sound
  // if audio is well initialized, or see a log in the console describing the issue.
  void beep () {
    if(auto a = Audio::getInstance()) {
        auto xfade = 401;
        auto c = a->out().openChannel(1.f,
                                      ChannelClosingPolicy::AutoClose,
                                      xfade);
        auto time_unit = 120;
        auto s = Do;
        auto half_tone = compute_half_tone(1.f);
        auto stereo_gain = stereo(0.f);
        auto volume = 0.6f;
        auto notespecs = parseMusic("do");
        using Request = Audio::Request;
        StackVector<Request> requests(notespecs.size());
        for(auto const & s : notespecs) {
            if(s.note == NOTE_ERROR) {
                LG(WARN,"parseMusic : note error");
                return;
            }
            auto half_tone = compute_half_tone(1.f);
            auto r = to_request<Audio::nAudioOut>(s,
                                                  time_unit,
                                                  1.f,
                                                  half_tone,
                                                  a->out().editSounds(),
                                                  MakeVolume::run<AudioOut::nAudioOut>(volume, stereo_gain));
            if(r.valid()) {
                requests.push_back(std::move(r));
            }
            else {
                LG(WARN,"parseMusic : invalid request was ignored");
            }
        }

        a->out().play(c, std::move(requests));
    }
    else {
      LG(WARN,"Audio is not initialized");
    }
  }
}

#endif
