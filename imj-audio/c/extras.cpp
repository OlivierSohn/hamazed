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
    constexpr int sz_one_shots = 400; // we need one to start or change or end a note.
    constexpr int sz_computes = 400; // we need one per synth.
    static Ctxt c(GlobalAudioLock<audioEnginePolicy>::get(),
                  sz_one_shots,
                  sz_computes);
    return c;
  }

  VoiceWindImpl & windVoice()
  {
    static VoiceWindImpl v;
    return v;
  }

} // NS imajuscule::audio

#endif
