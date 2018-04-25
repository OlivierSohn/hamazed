#ifdef __cplusplus

#include <iostream>

#if __APPLE__
#include <fenv.h>
#endif

#include "cpp.os.audio/include/public.h"

namespace imajuscule {
  namespace audioelement {
    template <typename Float>
    using VolumeAdjustedOscillator = FinalAudioElement<VolumeAdjusted<OscillatorAlgo<Float, eNormalizePolicy::FAST>>>;
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

    namespace sine {
      using AudioOutSynth = Synth <
        AudioOut::nAudioOut
      , XfadePolicy::UseXfade
      , MonoNoteChannel<1, audioelement::Oscillator<float>>
      , true
      , EventIterator<IEventList>
      , NoteOnEvent
      , NoteOffEvent>;

      AudioOutSynth & getSynth() {
        static AudioOutSynth s;
        return s;
      }

      void onSynthEvent(imajuscule::audio::Event const & e) {
        if(auto a = Audio::getInstance()) {
          // we need to check for closed() too, or else we should review the way note on/off are detected,
          // because it would probably cause bugs
          getSynth().onEvent(e, [](auto & c) { return c.elem.isInactive() && c.closed(); }, a->out().getChannelHandler());
        }
      }
    }

    namespace vasine {
      using AudioOutSynth = Synth <
        AudioOut::nAudioOut
      , XfadePolicy::UseXfade
      , MonoNoteChannel<1, audioelement::VolumeAdjustedOscillator<float>>
      , true
      , EventIterator<IEventList>
      , NoteOnEvent
      , NoteOffEvent>;

      AudioOutSynth & getSynth() {
        static AudioOutSynth s;
        return s;
      }

      void onSynthEvent(imajuscule::audio::Event const & e) {
        if(auto a = Audio::getInstance()) {
          // we need to check for closed() too, or else we should review the way note on/off are detected,
          // because it would probably cause bugs
          getSynth().onEvent(e, [](auto & c) { return c.elem.isInactive() && c.closed(); }, a->out().getChannelHandler());
        }
      }
    }
  }
}
namespace mySynth = imajuscule::audio::vasine;

extern "C" {

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

  void initializeAudio () {
    using namespace imajuscule;
    using namespace std;
#ifndef NDEBUG
    cout << "WARNING : C++ sources were built without NDEBUG" << endl;
#endif

    /*
    Denormals can appear in reverbs, when signal becomes close to 0.
    We disable denormals handling because the signal is too low for it to have any
    audible effect, and it is (said to be) very slow.
    */
    disableDenormals();

    // We know we're going to use audio so we force initialization.
    Audio::Init(Audio::OutInitPolicy::FORCE);
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

  void midiNoteOn(int pitch, float velocity) {
    using namespace imajuscule::audio;
    using namespace mySynth;
    onSynthEvent(mkNoteOn(pitch,velocity));
  }

  void midiNoteOff(int pitch) {
    using namespace imajuscule::audio;
    using namespace mySynth;
    onSynthEvent(mkNoteOff(pitch));
  }

  // Opens a 'Channel', and plays a Request in it.
  void beep () {
    if(auto a = Audio::getInstance()) {
      // copy/pasted parts of scriptinterpreter.cpp
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
            //float transpose_factor = expt(half_tone, 0);
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
