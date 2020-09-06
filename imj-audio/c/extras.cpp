#include "extras.h"

#ifdef __cplusplus


namespace imajuscule::audio {
    namespace audioelement {
        std::ostream & operator << (std::ostream & os, OscillatorType t) {
            switch(t) {
                case OscillatorType::SinusVolumeAdjusted:
                  os << "SinusVolumeAdjusted"; break;
                case OscillatorType::Sinus:
                  os << "Sinus"; break;
                case OscillatorType::Saw:
                  os << "Saw"; break;
                case OscillatorType::Square:
                  os << "Square"; break;
                case OscillatorType::Triangle:
                  os << "Triangle"; break;
                case OscillatorType::Noise:
                  os << "Noise"; break;
            }
            return os;
        }
    }
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

} // NS imajuscule::audio

#endif
