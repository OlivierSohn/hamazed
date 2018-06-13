#include "extras.h"

#ifdef __cplusplus

namespace imajuscule {
  namespace audioelement {
    float* analyzeEnvelopeGraph(envelType t, AHDSR p, int* nElems, int*splitAt) {
      switch(t) {
        case AHDSR_ReleaseAfterDecay:
          return envelopeGraph<AHDSREnvelope<float, EnvelopeRelease::ReleaseAfterDecay>>(p, nElems, splitAt);
        case AHDSR_WaitForKeyRelease:
          return envelopeGraph<AHDSREnvelope<float, EnvelopeRelease::WaitForKeyRelease>>(p, nElems, splitAt);
        default:
          return {};
      }
    }
  }
  namespace audio {

    Ctxt & getAudioContext() {
      static Ctxt c { GlobalAudioLock<AudioOutPolicy::Master>::get() };
      return c;
    }

    XFadeChans & getXfadeChannels() {
      return **(getAudioContext().getChannelHandler().getChannels().getChannelsXFade().begin());
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

    VoiceWindImpl & windVoice()
    {
        static constexpr auto n_mnc = VoiceWindImpl::n_channels;
        using mnc_buffer = VoiceWindImpl::MonoNoteChannel::buffer_t;
        static std::array<mnc_buffer, n_mnc> buffers;
        static VoiceWindImpl v(std::make_from_tuple<VoiceWindImpl>(buffers));
        return v;
    }


  } // NS audio

  namespace audioelement {

    void midiEventAHDSR(envelType t, AHDSR p, audio::Event n) {
      using namespace audio;
      switch(t) {
        case AHDSR_ReleaseAfterDecay:
          midiEvent<AHDSREnvelope<float, EnvelopeRelease::ReleaseAfterDecay>>(p, n);
          break;
        case AHDSR_WaitForKeyRelease:
          midiEvent<AHDSREnvelope<float, EnvelopeRelease::WaitForKeyRelease>>(p, n);
          break;
        default:
          break;
      }
    }

  } // NS audioelement

}

#endif
