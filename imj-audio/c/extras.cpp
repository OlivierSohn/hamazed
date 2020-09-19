#include "extras.h"

#ifdef __cplusplus


namespace imajuscule::audio {
    namespace audioelement {
        std::ostream & operator << (std::ostream & os, OscillatorType t) {
            switch(t) {
                case OscillatorType::SinusLoudnessVolumeAdjusted:
                  os << "SinusLoudnessVolumeAdjusted"; break;
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
                case OscillatorType::Sweep:
                  os << "Sweep"; break;
            }
            return os;
        }
    }
  Ctxt & getAudioContext() {
    static Ctxt c(SAMPLE_RATE);
    return c;
  }

  XFadeChans *& getXfadeChannels() {
    static XFadeChans * p = nullptr;
    return p;
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
