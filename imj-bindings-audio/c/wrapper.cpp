#include "compiler.prepro.h"
#include "extras.h"

#ifdef __cplusplus

namespace imajuscule {
    namespace audio {
        // explicit instantiation to attempt to fix the symbol not found error
        template void playOneThing(VoiceWindImpl&a, ChannelHandler&b, XFadeChans&c, Voicing const &d);
    }
}

extern "C" {

  void testFreeList() {
    using FL = imajuscule::FreeList<int64_t, 4096/sizeof(int64_t)>;
    FL l;
    l.Take();
// should the size of the free list be limited to a page/ aligned to the start of the page?
    using namespace imajuscule::audioelement;
    using namespace imajuscule::audio::mySynth;
    using namespace imajuscule::audio;
    std::cout << "sizeof synth " << sizeof(SynthT<AHDSREnvelope<float, EnvelopeRelease::ReleaseAfterDecay>>) << std::endl;
    std::cout << "sizeof mnc " << sizeof(MonoNoteChannel<VolumeAdjustedOscillator<AHDSREnvelope<float, EnvelopeRelease::ReleaseAfterDecay>>>) << std::endl;
    std::cout << "sizeof au " << sizeof(AEBuffer<float>) << std::endl;
    std::cout << "sizeof aub " << sizeof(AEBuffer<float>::buffer_placeholder_t) << std::endl;
    union {
        AEBuffer<float>::buffer_placeholder_t for_alignment;
        float buffer[n_frames_per_buffer];
    } u;
    std::cout << "sizeof auu " << sizeof(u) << std::endl;

    struct F { // 64 bytes
      union {
          AEBuffer<float>::buffer_placeholder_t for_alignment;
          float buffer[n_frames_per_buffer];
      };
    };
    struct G_ { // 128 bytes
      union {
          AEBuffer<float>::buffer_placeholder_t for_alignment;
          float buffer[n_frames_per_buffer];
      };
      bool me : 1;
    };
    struct G { // 128 bytes
      union {
          AEBuffer<float>::buffer_placeholder_t for_alignment;
          float buffer[n_frames_per_buffer];
      };
      bool me : 1;
    }__attribute__((packed));
    struct G2 { // 128 bytes
      union {
          AEBuffer<float>::buffer_placeholder_t for_alignment;
          float buffer[n_frames_per_buffer];
      };
      bool me;
    }__attribute__((packed));
    struct g2_ : public G2 {
      int32_t i;
    }__attribute__((packed));
    struct g2 : public G2 {
      int32_t i : 3;
    }__attribute__((packed));

    struct H { // 68 bytes
      union {
          float buffer[n_frames_per_buffer];
      };
      bool me : 1;
    };
    struct H2 { // 68 bytes
      float buffer[n_frames_per_buffer];
      bool me : 1;
    };
    struct I { // 128 bytes
      union {
          AEBuffer<float>::buffer_placeholder_t for_alignment;
      };
      bool me : 1;
    };
    struct I2 { // 128 bytes
      AEBuffer<float>::buffer_placeholder_t for_alignment;
      bool me : 1;
    };
    struct i2_ : public I2 {
      int stuff;
    };
    struct i2 : public I2 {
      int stuff;
    }__attribute__((packed));
    struct ii2 : public i2 {
      int stuff2;
    };

    std::cout << "sizeof F " << sizeof(F) << std::endl;
    std::cout << "sizeof G_ " << sizeof(G_) << std::endl;
    std::cout << "sizeof G " << sizeof(G) << std::endl;
    std::cout << "sizeof G2 " << sizeof(G2) << std::endl;
    std::cout << "sizeof g2_ " << sizeof(g2_) << std::endl;
    std::cout << "sizeof g2 " << sizeof(g2) << std::endl;
    std::cout << "sizeof H " << sizeof(H) << std::endl;
    std::cout << "sizeof H2 " << sizeof(H2) << std::endl;
    std::cout << "sizeof I " << sizeof(I) << std::endl;
    std::cout << "sizeof I2 " << sizeof(I2) << std::endl;
    std::cout << "sizeof i2_ " << sizeof(i2_) << std::endl;
    std::cout << "sizeof i2 " << sizeof(i2) << std::endl;
    std::cout << "sizeof ii2 " << sizeof(ii2) << std::endl;

    std::cout << "sizeof NoXFadeChans " << sizeof(NoXFadeChans) << std::endl;
  }
  
  bool initializeAudio () {
    using namespace std;
    using namespace imajuscule;
    using namespace imajuscule::audio;
#ifndef NDEBUG
    cout << "Warning : C++ sources of imj-bindings-audio were built without NDEBUG" << endl;
#endif
    disableDenormals();

    //testFreeList();

    setPortaudioEnvVars();

    // add a single Xfade channel (needed because soundengine and channel don't support envelopes entirely)
    static constexpr auto n_max_orchestrator_per_channel = 1;
    {
      auto p = std::make_unique<XFadeChans>(
        getAudioContext().getChannelHandler().get_lock_policy(),
        std::numeric_limits<uint8_t>::max(),
        n_max_orchestrator_per_channel);
      {
        AudioFreeze l(getAudioContext().getChannelHandler().get_lock());

        getAudioContext().getChannelHandler().getChannels().getChannelsXFade().emplace_back(std::move(p));
        getAudioContext().getChannelHandler().getChannels().getChannelsNoXFade().reserve(100);
      }
    }

    windVoice().initializeSlow();
    if(!windVoice().initialize(getXfadeChannels())) {
      LG(ERR,"windVoice().initialize failed");
      return false;
    }

    getAudioContext().Init();

    if(imajuscule::thread::priorityIsReadOnly()) {
        cout << endl;
        cout << "Warning :" << endl;
        cout << "  The audio engine needs to be able to dynamically change a thread priority" << endl;
        cout << "  to avoid priority inversion when holding the audio lock." << endl;
        cout << "  We detected that your system doesn't allow setting thread priorities, hence" << endl;
        cout << "  you may occasionally hear some audio clics/cracks. To fix this, you can" << endl;
        cout << "  run the command again using 'sudo' : root privileges are required on Linux" << endl;
        cout << "  to use 'pthread_setschedparam'." << endl;
        cout << endl;
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

    windVoice().finalize(getXfadeChannels());
    
    Synths<SimpleLinearEnvelope<float>>::finalize();
    Synths<AHDSREnvelope<float, EnvelopeRelease::WaitForKeyRelease>>::finalize();
    Synths<AHDSREnvelope<float, EnvelopeRelease::ReleaseAfterDecay>>::finalize();

    getAudioContext().TearDown();

    {
      AudioFreeze l(getAudioContext().getChannelHandler().get_lock());

      getAudioContext().getChannelHandler().getChannels().getChannelsXFade().clear();
      getAudioContext().getChannelHandler().getChannels().getChannelsNoXFade().clear();
    }
  }

  void midiNoteOn(int envelCharacTime, int16_t pitch, float velocity) {
    using namespace imajuscule::audio;
    using namespace imajuscule::audioelement;
    midiEvent<SimpleLinearEnvelope<float>>(envelCharacTime, mkNoteOn(pitch,velocity));
  }
  void midiNoteOff(int envelCharacTime, int16_t pitch) {
    using namespace imajuscule::audio;
    using namespace imajuscule::audioelement;
    midiEvent<SimpleLinearEnvelope<float>>(envelCharacTime, mkNoteOff(pitch));
  }

  void midiNoteOnAHDSR_(envelType t, int a, int ai, int h, int d, int di, float s, int r, int ri, int16_t pitch, float velocity) {     
    using namespace imajuscule;
    using namespace imajuscule::audio;
    using namespace imajuscule::audioelement;
    auto p = AHDSR{a,itp::toItp(ai),h,d,itp::toItp(di),r,itp::toItp(ri),s};
    auto n = mkNoteOn(pitch,velocity);
    midiEventAHDSR(t, p, n);
  }
  void midiNoteOffAHDSR_(envelType t, int a, int ai, int h, int d, int di, float s, int r, int ri, int16_t pitch) {
    using namespace imajuscule;
    using namespace imajuscule::audio;
    using namespace imajuscule::audioelement;
    auto p = AHDSR{a,itp::toItp(ai),h,d,itp::toItp(di),r,itp::toItp(ri),s};
    auto n = mkNoteOff(pitch);
    midiEventAHDSR(t, p, n);
  }

  float* analyzeAHDSREnvelope_(envelType t, int a, int ai, int h, int d, int di, float s, int r, int ri, int*nElems, int*splitAt) {
    using namespace imajuscule;
    using namespace imajuscule::audio;
    using namespace imajuscule::audioelement;
    auto p = AHDSR{a,itp::toItp(ai),h,d,itp::toItp(di),r,itp::toItp(ri),s};
    return analyzeEnvelopeGraph(t, p, nElems, splitAt);
  }

  void effectOn(int program, int16_t pitch, float velocity) {
    using namespace imajuscule::audio;
    auto voicing = Voicing(program,pitch,velocity,0.f,true,0);
    playOneThing(windVoice(),getAudioContext().getChannelHandler(),getXfadeChannels(),voicing);
  }

  void effectOff(int16_t pitch) {
    using namespace imajuscule::audio;
    stopPlaying(windVoice(),getAudioContext().getChannelHandler(),getXfadeChannels(),pitch);
  }
}

#endif
