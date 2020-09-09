#include "compiler.prepro.h"
#include "extras.h"
#include "memory.h"

#define IMJ_TRACE_EXTERN_C 0

#ifdef __cplusplus

namespace imajuscule::audio {
  /*
  * Is equal to:
  *   number of calls to 'initializeAudioOutput' - number of calls to 'teardownAudioOutput'
  */
  int & countUsers() {
    static int n(0);
    return n;
  }

  /*
  * Protects access to countUsers() and the initialization / uninitialization of the global audio output stream
  */
  std::mutex & initMutex() {
    static std::mutex m;
    return m;
  }

  bool convert(onEventResult e) {
    switch(e) {
      case onEventResult::OK:
        return true;
      default:
        return false;
    }
  }
}

namespace imajuscule::audio::audioelement {


  template<typename Env>
  double* envelopeGraph(typename Env::Param const & rawEnvParams, int*nElems, int*splitAt) {
    std::vector<double> v;
    int split;
    std::tie(v, split) = envelopeGraphVec<Env>(rawEnvParams);
#if 0
    // for debugging purposes
    LG(INFO, "writing envelope as .wav file");
    write_wav("/Users/Olivier/Dev/hs.hamazed", "env.wav", std::vector<std::vector<double>>{v}, 100);
#endif
    if(nElems) {
      *nElems = v.size();
    }
    if(splitAt) {
      *splitAt = split;
    }
    auto n_bytes = v.size()*sizeof(decltype(v[0]));
    auto c_arr = imj_c_malloc(n_bytes); // will be freed by haskell finalizer.
    memcpy(c_arr, v.data(), n_bytes);
    return static_cast<double*>(c_arr);
  }

  double* analyzeEnvelopeGraph(EnvelopeRelease t, AHDSR p, int* nElems, int*splitAt) {
    static constexpr auto A = getAtomicity<audio::Ctxt::policy>();
    switch(t) {
      case EnvelopeRelease::ReleaseAfterDecay:
        return envelopeGraph<AHDSREnvelope<A, AudioFloat, EnvelopeRelease::ReleaseAfterDecay>>(p, nElems, splitAt);
      case EnvelopeRelease::WaitForKeyRelease:
        return envelopeGraph<AHDSREnvelope<A, AudioFloat, EnvelopeRelease::WaitForKeyRelease>>(p, nElems, splitAt);
      default:
        return {};
    }
  }

  audio::onEventResult midiEventAHDSR(OscillatorType osc, EnvelopeRelease t,
                                      CConstArray<harmonicProperties_t> const & harmonics,
                                      AHDSR p, audio::Event n, Optional<audio::MIDITimestampAndSource> maybeMts) {
    static constexpr auto A = getAtomicity<audio::Ctxt::policy>();
    switch(t) {
      case EnvelopeRelease::ReleaseAfterDecay:
        return midiEvent_<AHDSREnvelope<A, AudioFloat, EnvelopeRelease::ReleaseAfterDecay>>(osc, harmonics, p, n, maybeMts);
      case EnvelopeRelease::WaitForKeyRelease:
        return midiEvent_<AHDSREnvelope<A, AudioFloat, EnvelopeRelease::WaitForKeyRelease>>(osc, harmonics, p, n, maybeMts);
    }
  }

  audio::onEventResult midiEventAHDSRSweep(OscillatorType osc, EnvelopeRelease t,
                                           CConstArray<harmonicProperties_t> const & harmonics,
                                           AHDSR p,
                                           audioelement::SweepSetup const & sweep,
                                           audio::Event n, Optional<audio::MIDITimestampAndSource> maybeMts) {
    static constexpr auto A = getAtomicity<audio::Ctxt::policy>();
    switch(t) {
      case EnvelopeRelease::ReleaseAfterDecay:
        return midiEventSweep_<AHDSREnvelope<A, AudioFloat, EnvelopeRelease::ReleaseAfterDecay>>(osc, harmonics, p, sweep, n, maybeMts);
      case EnvelopeRelease::WaitForKeyRelease:
        return midiEventSweep_<AHDSREnvelope<A, AudioFloat, EnvelopeRelease::WaitForKeyRelease>>(osc, harmonics, p, sweep, n, maybeMts);
    }
  }

} // NS imajuscule::audio::audioelement


struct Trace {
    Trace(std::string const & name)
    : name(name) {
        std::cout << ">>> " << name << std::endl;
    }
    ~Trace() {
        std::cout << "<<< " << name << std::endl;
    }
private:
    std::string name;
};

harmonicProperties_t const * getUnityHarmonics() {
     static harmonicProperties_t har[1]{
             {0.f, 1.f}
     };
     return har;
}

extern "C" {

  /*
  * Increments the count of users, and
  *
  * - If we are the first user:
  *     initializes the audio output context,
  *     taking into account the latency parameters,
  * - Else:
  *     returns the result of the first initialization,
  *     ignoring the latency parameters.
  *
  * Every successfull or unsuccessfull call to this function
  *   should be matched by a call to 'teardownAudioOutput'.
  *   .
  * @param minLatencySeconds :
  *   The minimum portaudio latency, in seconds.
  *   Pass 0.f to use the smallest latency possible.
  * @param portaudioMinLatencyMillis :
  *   If strictly positive, overrides the portaudio minimum latency by
  *     setting an environment variable.
  *
  * @returns true on success, false on error.
  */
  bool initializeAudioOutput (float minLatencySeconds, int portaudioMinLatencyMillis) {
#if IMJ_TRACE_EXTERN_C
    Trace trace("initializeAudioOutput");
    std::cout << minLatencySeconds << " " << portaudioMinLatencyMillis << std::endl;
#endif
    using namespace std;
    using namespace imajuscule;
    using namespace imajuscule::audio;

    std::lock_guard l(initMutex());
    ++countUsers();
    LG(INFO, "initializeAudioOutput: nUsers = %d", countUsers());

    {
      if( countUsers() > 1) {
        // We are ** not ** the first user.
        return getAudioContext().Initialized();
      }
      else if(countUsers() <= 0) {
        LG(ERR, "initializeAudioOutput: nUsers = %d", countUsers());
        Assert(0);
        return getAudioContext().Initialized();
      }
    }

#ifndef NDEBUG
    cout << "Warning : C++ sources of imj-audio were built without NDEBUG" << endl;
#endif

#ifdef IMJ_AUDIO_MASTERGLOBALLOCK
    cout << "Warning : C++ sources of imj-audio were built with IMJ_AUDIO_MASTERGLOBALLOCK. " <<
    "This may lead to audio glitches under contention." << endl;
#endif

    if(portaudioMinLatencyMillis > 0) {
      if(!overridePortaudioMinLatencyMillis(portaudioMinLatencyMillis)) {
        return false;
      }
    }

    disableDenormals();

    //testFreeList();

    // add a single Xfade channel (for 'SoundEngine' and 'Channel' that don't support envelopes entirely)
    static constexpr auto n_max_orchestrator_per_channel = 1;
    auto [xfadeChan, _] = getAudioContext().getChannelHandler().getChannels().getChannelsXFade().emplace_front(
      getAudioContext().getChannelHandler().get_lock_policy(),
      std::numeric_limits<uint8_t>::max(),
      n_max_orchestrator_per_channel);

    windVoice().initializeSlow();
    if(!windVoice().initialize(xfadeChan)) {
      LG(ERR,"windVoice().initialize failed");
      return false;
    }
    getXfadeChannels() = &xfadeChan;

    if(!getAudioContext().Init(minLatencySeconds)) {
      return false;
    }

    {
      auto & d = midiDelays(); // to allocate the static inside
      if(d.empty()) {
        LG(ERR, "empty midi delays");
      }
      else if(d[0].get()) {
        LG(ERR, "wrong midi delays initilization");
      }
    }

    // On macOS 10.13.5, this delay is necessary to be able to play sound,
    //   it might be a bug in portaudio where Pa_StartStream doesn't wait for the
    //   stream to be up and running.
    std::this_thread::sleep_for(std::chrono::milliseconds(1000));
    return true;
  }

  /*
  * Decrements the count of users, and if we are the last user,
  *   shutdowns audio output after having driven the audio signal to 0.
  *
  * Every successfull or unsuccessfull call to 'initializeAudioOutput'
  * must be matched by a call to this function.
  */
  void teardownAudioOutput() {
#if IMJ_TRACE_EXTERN_C
    Trace trace("teardownAudioOutput");
#endif
    using namespace imajuscule;
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::audioelement;

    std::lock_guard l(initMutex());

    --countUsers();
    LG(INFO, "teardownAudioOutput  : nUsers = %d", countUsers());
    if(countUsers() > 0) {
      // We are ** not ** the last user.
      return;
    }

    if(getAudioContext().Initialized()) {
      // This will "quickly" crossfade the audio output channels to zero.
      getAudioContext().onApplicationShouldClose();

      // we sleep whil channels are crossfaded to zero

      int bufferSize = n_audio_cb_frames.load(std::memory_order_relaxed);
      if(bufferSize == initial_n_audio_cb_frames) {
        // assume a very big buffer size if the audio callback didn't have a chance
        // to run yet.
        bufferSize = 10000;
      }
      float latencyTime = bufferSize / static_cast<float>(SAMPLE_RATE);
      float fadeOutTime = xfade_on_close / static_cast<float>(SAMPLE_RATE);
      float marginTimeSeconds = 0.020f; // taking into account the time to run the code
      float waitSeconds = 2*latencyTime + 2*fadeOutTime + marginTimeSeconds;
      std::this_thread::sleep_for( std::chrono::milliseconds(1 + static_cast<int>(waitSeconds * 1000)));
    }

    // All channels have crossfaded to 0 by now.

    windVoice().finalize();

    foreachOscillatorType<FinalizeSynths>();

    getAudioContext().TearDown();

    getAudioContext().getChannelHandler().getChannels().getChannelsXFade().clear();
    getAudioContext().getChannelHandler().getChannels().getChannelsNoXFade().clear();
  }

  void setMaxMIDIJitter(uint64_t v) {
#if IMJ_TRACE_EXTERN_C
    Trace trace("setMaxMIDIJitter");
    std::cout << v << std::endl;
#endif
    using namespace imajuscule::audio;
    maxMIDIJitter() = v;
  }

  bool midiNoteOnAHDSR_(imajuscule::audio::audioelement::OscillatorType osc,
                        imajuscule::audio::audioelement::EnvelopeRelease t,
                       int a, int ai, int h, int d, int di, float s, int r, int ri,
                       harmonicProperties_t const * hars, int har_sz,
                       int16_t pitch, float velocity, int midiSource, uint64_t maybeMIDITime) {
#if IMJ_TRACE_EXTERN_C
    Trace trace("midiNoteOnAHDSR_");
    std::cout << osc << std::endl;
    std::cout << t << std::endl;
    std::cout << a << std::endl;
    std::cout << ai << std::endl;
    std::cout << h << std::endl;
    std::cout << d << std::endl;
    std::cout << di << std::endl;
    std::cout << s << std::endl;
    std::cout << r << std::endl;
    std::cout << ri << std::endl;
    std::cout << har_sz << std::endl;
    std::cout << pitch << std::endl;
    std::cout << velocity << std::endl;
    std::cout << midiSource << std::endl;
    std::cout << maybeMIDITime << std::endl;
#endif
    using namespace imajuscule;
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::audioelement;
    if(unlikely(!getAudioContext().Initialized())) {
      return false;
    }
    auto p = AHDSR{a,itp::toItp(ai),h,d,itp::toItp(di),r,itp::toItp(ri),s};
    auto n = mkNoteOn(pitch,velocity);
    auto maybeMts = (midiSource >= 0) ?
      Optional<MIDITimestampAndSource>{{maybeMIDITime, static_cast<uint64_t>(midiSource)}} :
      Optional<MIDITimestampAndSource>{};
    if (osc <= OscillatorType::Noise) {
        har_sz = 1;
        hars = getUnityHarmonics();
    }
    return convert(midiEventAHDSR(osc, t, {hars, har_sz}, p, n, maybeMts));
  }

  bool midiNoteOnAHDSRSweep_(imajuscule::audio::audioelement::OscillatorType osc,
                        imajuscule::audio::audioelement::EnvelopeRelease t,
                       int a, int ai, int h, int d, int di, float s, int r, int ri,
                       harmonicProperties_t const * hars, int har_sz,
                       int sweep_duration,
                       float sweep_freq,
                       imajuscule::audio::audioelement::Extremity sweep_freq_extremity,
                       int16_t pitch, float velocity, int midiSource, uint64_t maybeMIDITime) {
#if IMJ_TRACE_EXTERN_C
    Trace trace("midiNoteOnAHDSRSweep_");
    std::cout << osc << std::endl;
    std::cout << t << std::endl;
    std::cout << a << std::endl;
    std::cout << ai << std::endl;
    std::cout << h << std::endl;
    std::cout << d << std::endl;
    std::cout << di << std::endl;
    std::cout << s << std::endl;
    std::cout << r << std::endl;
    std::cout << ri << std::endl;
    std::cout << har_sz << std::endl;
    std::cout << sweep_duration << std::endl;
    std::cout << sweep_freq << std::endl;
    std::cout << pitch << std::endl;
    std::cout << velocity << std::endl;
    std::cout << midiSource << std::endl;
    std::cout << maybeMIDITime << std::endl;
#endif
    using namespace imajuscule;
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::audioelement;
    if(unlikely(!getAudioContext().Initialized())) {
      return false;
    }
    auto p = AHDSR{a,itp::toItp(ai),h,d,itp::toItp(di),r,itp::toItp(ri),s};
    auto n = mkNoteOn(pitch,velocity);
    auto maybeMts = (midiSource >= 0) ?
      Optional<MIDITimestampAndSource>{{maybeMIDITime, static_cast<uint64_t>(midiSource)}} :
      Optional<MIDITimestampAndSource>{};
    if (osc <= OscillatorType::Noise) {
        har_sz = 1;
        hars = getUnityHarmonics();
    }
    return convert(midiEventAHDSRSweep(osc, t, {hars, har_sz}, p, SweepSetup{sweep_duration, sweep_freq, sweep_freq_extremity}, n, maybeMts));
  }

  bool midiNoteOffAHDSR_(imajuscule::audio::audioelement::OscillatorType osc,
                         imajuscule::audio::audioelement::EnvelopeRelease t,
                         int a, int ai, int h, int d, int di, float s, int r, int ri,
                         harmonicProperties_t const * hars, int har_sz,
                         int16_t pitch, int midiSource, uint64_t maybeMIDITime) {
#if IMJ_TRACE_EXTERN_C
    Trace trace("midiNoteOffAHDSR_");
    std::cout << osc << std::endl;
    std::cout << t << std::endl;
    std::cout << a << std::endl;
    std::cout << ai << std::endl;
    std::cout << h << std::endl;
    std::cout << d << std::endl;
    std::cout << di << std::endl;
    std::cout << s << std::endl;
    std::cout << r << std::endl;
    std::cout << ri << std::endl;
    std::cout << har_sz << std::endl;
    std::cout << pitch << std::endl;
    std::cout << midiSource << std::endl;
    std::cout << maybeMIDITime << std::endl;
#endif
    using namespace imajuscule;
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::audioelement;
    if(unlikely(!getAudioContext().Initialized())) {
      return false;
    }
    auto p = AHDSR{a,itp::toItp(ai),h,d,itp::toItp(di),r,itp::toItp(ri),s};
    auto n = mkNoteOff(pitch);
    auto maybeMts = (midiSource >= 0) ?
      Optional<MIDITimestampAndSource>{{maybeMIDITime, static_cast<uint64_t>(midiSource)}} :
      Optional<MIDITimestampAndSource>{};
    if (osc <= OscillatorType::Noise) {
        har_sz = 1;
        hars = getUnityHarmonics();
    }
    return convert(midiEventAHDSR(osc, t, {hars, har_sz}, p, n, maybeMts));
  }

  bool midiNoteOffAHDSRSweep_(imajuscule::audio::audioelement::OscillatorType osc,
                         imajuscule::audio::audioelement::EnvelopeRelease t,
                         int a, int ai, int h, int d, int di, float s, int r, int ri,
                         harmonicProperties_t const * hars, int har_sz,
                         int sweep_duration,
                         float sweep_freq,
                         imajuscule::audio::audioelement::Extremity sweep_freq_extremity,
                         int16_t pitch, int midiSource, uint64_t maybeMIDITime) {
#if IMJ_TRACE_EXTERN_C
    Trace trace("midiNoteOffAHDSRSweep_");
    std::cout << osc << std::endl;
    std::cout << t << std::endl;
    std::cout << a << std::endl;
    std::cout << ai << std::endl;
    std::cout << h << std::endl;
    std::cout << d << std::endl;
    std::cout << di << std::endl;
    std::cout << s << std::endl;
    std::cout << r << std::endl;
    std::cout << ri << std::endl;
    std::cout << har_sz << std::endl;
    std::cout << sweep_duration << std::endl;
    std::cout << sweep_freq << std::endl;
    std::cout << sweep_freq_extremity << std::endl;
    std::cout << pitch << std::endl;
    std::cout << midiSource << std::endl;
    std::cout << maybeMIDITime << std::endl;
#endif
    using namespace imajuscule;
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::audioelement;
    if(unlikely(!getAudioContext().Initialized())) {
      return false;
    }
    auto p = AHDSR{a,itp::toItp(ai),h,d,itp::toItp(di),r,itp::toItp(ri),s};
    auto n = mkNoteOff(pitch);
    auto maybeMts = (midiSource >= 0) ?
      Optional<MIDITimestampAndSource>{{maybeMIDITime, static_cast<uint64_t>(midiSource)}} :
      Optional<MIDITimestampAndSource>{};
    if (osc <= OscillatorType::Noise) {
        har_sz = 1;
        hars = getUnityHarmonics();
    }
    return convert(midiEventAHDSRSweep(osc, t, {hars, har_sz}, p, SweepSetup{sweep_duration, sweep_freq, sweep_freq_extremity}, n, maybeMts));
  }

  double* analyzeAHDSREnvelope_(imajuscule::audio::audioelement::EnvelopeRelease t, int a, int ai, int h, int d, int di, float s, int r, int ri, int*nElems, int*splitAt) {
#if IMJ_TRACE_EXTERN_C
    Trace trace("analyzeAHDSREnvelope_");
#endif
    using namespace imajuscule;
    using namespace imajuscule::audio;
    using namespace imajuscule::audio::audioelement;
    auto p = AHDSR{a,itp::toItp(ai),h,d,itp::toItp(di),r,itp::toItp(ri),s};
    return analyzeEnvelopeGraph(t, p, nElems, splitAt);
  }

  bool effectOn(int program, int16_t pitch, float velocity) {
#if IMJ_TRACE_EXTERN_C
    Trace trace("effectOn");
#endif
    using namespace imajuscule::audio;
    if(unlikely(!getAudioContext().Initialized())) {
      return false;
    }
    auto voicing = Voicing(program,pitch,velocity,0.f,true,0);
    return convert(playOneThing(windVoice(),getAudioContext().getChannelHandler(),*getXfadeChannels(),voicing));
  }

  bool effectOff(int16_t pitch) {
#if IMJ_TRACE_EXTERN_C
    Trace trace("effectOff");
#endif
    using namespace imajuscule::audio;
    if(unlikely(!getAudioContext().Initialized())) {
      return false;
    }
    return convert(stopPlaying(windVoice(),getAudioContext().getChannelHandler(),*getXfadeChannels(),pitch));
  }

  bool getConvolutionReverbSignature_(const char * dirPath, const char * filePath, spaceResponse_t * r) {
#if IMJ_TRACE_EXTERN_C
    Trace trace("getConvolutionReverbSignature_");
#endif
    using namespace imajuscule::audio;
    return getConvolutionReverbSignature(dirPath, filePath, *r);
  }

  bool dontUseReverb_() {
#if IMJ_TRACE_EXTERN_C
    Trace trace("dontUseReverb_");
#endif
    using namespace imajuscule::audio;
    if(unlikely(!getAudioContext().Initialized())) {
      return false;
    }
    dontUseConvolutionReverbs(getAudioContext().getChannelHandler());
    return true;
  }
  bool useReverb_(const char * dirPath, const char * filePath) {
#if IMJ_TRACE_EXTERN_C
    Trace trace("useReverb_");
    std::cout << dirPath << " " << filePath << std::endl;
#endif
    using namespace imajuscule::audio;
    if(unlikely(!getAudioContext().Initialized())) {
      return false;
    }
    return useConvolutionReverb(getAudioContext().getChannelHandler().getPost(), dirPath, filePath);
  }
  bool setReverbWetRatio(double wet) {
#if IMJ_TRACE_EXTERN_C
    Trace trace("setReverbWetRatio");
    std::cout << wet << std::endl;
#endif
    using namespace imajuscule::audio;
    if(unlikely(!getAudioContext().Initialized())) {
      return false;
    }
    getAudioContext().getChannelHandler().enqueueOneShot([wet](auto & chans) {
      chans.getPost().transitionConvolutionReverbWetRatio(wet);
    });
    return true;
  }
}

#endif
