#include "extras.h"

#ifdef __cplusplus

namespace imajuscule {
  namespace audioelement {
    float* analyzeEnvelopeGraph(envelType t, AHDSR p, int* nElems, int*splitAt) {
      static constexpr auto A = getAtomicity<audio::Ctxt::policy>();
      switch(t) {
        case envelType::AHDSR_ReleaseAfterDecay:
          return envelopeGraph<AHDSREnvelope<A, float, EnvelopeRelease::ReleaseAfterDecay>>(p, nElems, splitAt);
        case envelType::AHDSR_WaitForKeyRelease:
          return envelopeGraph<AHDSREnvelope<A, float, EnvelopeRelease::WaitForKeyRelease>>(p, nElems, splitAt);
        default:
          return {};
      }
    }
  }
  namespace audio {

    Ctxt & getAudioContext() {
      static Ctxt c;
      return c;
    }

    XFadeChans *& getXfadeChannels() {
      static XFadeChans * p = nullptr;
      return p;
    }

    Event mkNoteOn(int pitch, float velocity) {
      Event e;
      e.type = Event::kNoteOnEvent;
      e.noteOn.pitch = pitch;
      e.noteOn.velocity = velocity;
      e.noteOn.channel= 0; // unused
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
        static VoiceWindImpl v(buffers);
        return v;
    }

    bool convert(onEventResult e) {
      switch(e) {
        case onEventResult::OK:
          return true;
        default:
          return false;
      }
    }
  } // NS audio

  namespace audioelement {

    audio::onEventResult midiEventAHDSR(envelType t, AHDSR p, audio::Event n) {
      using namespace audio;
      static constexpr auto A = getAtomicity<audio::Ctxt::policy>();
      switch(t) {
        case envelType::AHDSR_ReleaseAfterDecay:
          return midiEvent<AHDSREnvelope<A, float, EnvelopeRelease::ReleaseAfterDecay>>(p, n);
        case envelType::AHDSR_WaitForKeyRelease:
          return midiEvent<AHDSREnvelope<A, float, EnvelopeRelease::WaitForKeyRelease>>(p, n);
        default:
        Assert(0);
        return onEventResult::DROPPED_NOTE;
      }
    }

  } // NS audioelement

}

#endif
