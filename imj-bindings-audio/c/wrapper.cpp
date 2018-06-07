#ifdef __cplusplus

#include "cpp.audio/include/public.h"

extern "C" {
  enum envelType {
      AHDSR_WaitForKeyRelease
    , AHDSR_ReleaseAfterDecay
  };
}

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

    template<typename S>
    struct ClampParam;
    template<typename S>
    struct SetParam;
    template<typename S>
    struct HasNoteOff;

    template<typename T>
    struct ClampParam<SimpleLinearEnvelope<T>> {
      static auto clamp(int envelCharacTime) {
        return std::max(envelCharacTime, 100);
      }
    };

    template<typename T, EnvelopeRelease Rel>
    struct ClampParam<AHDSREnvelope<T, Rel>> {
      static auto clamp(AHDSR const & env) {
        return env; // TODO clamp according to AHDSREnvelope
      }
    };

    template<typename T>
    struct SetParam<SimpleLinearEnvelope<T>> {
      template<typename A>
      static void set(int dt, A & a) {
        a.forEachElems([dt](auto & e) { e.algo.editEnveloppe().setEnvelopeCharacTime(dt); });
      }
    };

    template<typename T, EnvelopeRelease Rel>
    struct SetParam<AHDSREnvelope<T, Rel>> {
      template<typename A>
      static void set(AHDSR const & env, A & a) {
        a.forEachElems([&env](auto & e) { e.algo.editEnveloppe().setAHDSR(env); });
      }
    };

    template<typename T>
    struct HasNoteOff<SimpleLinearEnvelope<T>> {
      static constexpr bool value = true;
    };

    template<typename T, EnvelopeRelease Rel>
    struct HasNoteOff<AHDSREnvelope<T, Rel>> {
      static constexpr bool value = Rel == EnvelopeRelease::WaitForKeyRelease;
    };


    template<typename Env>
    std::pair<std::vector<float>, int> envelopeGraphVec(typename Env::Param const & rawEnvParams) {
      Env e;
      auto envParams = ClampParam<Env>::clamp(rawEnvParams);
      e.setAHDSR(envParams);
      // emulate a key-press
      e.onKeyPressed();
      int splitAt = -1;

      std::vector<float> v, v2;
      v.reserve(10000);
      for(int i=0; e.getState() != EnvelopeState::EnvelopeDone1; ++i) {
        e.step();
        v.push_back(e.value());
        if(!e.afterAttackBeforeSustain()) {
          splitAt = v.size();
          if constexpr (Env::Release == EnvelopeRelease::WaitForKeyRelease) {
            // emulate a key-release
            e.onKeyReleased();
          }
          break;
        }
      }
      while(e.getState() != EnvelopeState::EnvelopeDone1) {
        e.step();
        v.push_back(e.value());
      }
      return {std::move(v),splitAt};
    }

    template<typename Env>
    float* envelopeGraph(typename Env::Param const & rawEnvParams, int*nElems, int*splitAt) {
      std::vector<float> v;
      int split;
      std::tie(v, split) = envelopeGraphVec<Env>(rawEnvParams);
      if(nElems) {
        *nElems = v.size();
      }
      if(splitAt) {
        *splitAt = split;
      }
      auto n_bytes = v.size()*sizeof(decltype(v[0]));
      auto c_arr = malloc(n_bytes); // will be freed by haskell finalizer.
      memcpy(c_arr, v.data(), n_bytes);
      return static_cast<float*>(c_arr);
    }

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

    using AllChans = ChannelsVecAggregate< 2, AudioOutPolicy::Master >;

    using NoXFadeChans = typename AllChans::NoXFadeChans;
    using XFadeChans = typename AllChans::XFadeChans;

    using Ctxt = AudioOutContext<
      outputDataBase<
        AudioOutPolicy::Master,
        AllChans
        >,
      Features::JustOut,
      AudioPlatform::PortAudio
      >;

    using AudioFreeze = typename Ctxt::LockFromNRT;

    auto & getAudioContext() {
      static Ctxt c { GlobalAudioLock<AudioOutPolicy::Master>::get() };
      return c;
    }

    auto & getXfadeChannels() {
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

    auto & windVoice () {
      constexpr auto n_audio_out = 2;
      constexpr bool withNoteOff = true;
      using namespace audioelement;
      using VoiceWindImpl = Voice<n_audio_out, audio::SoundEngineMode::WIND, withNoteOff>;
      static constexpr auto n_mnc = VoiceWindImpl::n_channels;
      using mnc_buffer = VoiceWindImpl::MonoNoteChannel::buffer_t;
      static std::array<mnc_buffer,n_mnc> buffers;
      static VoiceWindImpl v(std::make_from_tuple<VoiceWindImpl>(buffers));
      return v;
    }

    namespace sine {
      template <typename Env>
      using SynthT = Synth <
        Ctxt::nAudioOut
      , XfadePolicy::SkipXfade
      , MonoNoteChannel<audioelement::Oscillator<Env>>
      , audioelement::HasNoteOff<Env>::value
      , EventIterator<IEventList>
      , NoteOnEvent
      , NoteOffEvent>;
    }

    namespace vasine {
      template <typename Env>
      using SynthT = Synth <
        Ctxt::nAudioOut
      , XfadePolicy::SkipXfade
      , MonoNoteChannel<audioelement::VolumeAdjustedOscillator<Env>>
      , audioelement::HasNoteOff<Env>::value
      , EventIterator<IEventList>
      , NoteOnEvent
      , NoteOffEvent>;
    }
    namespace mySynth = imajuscule::audio::vasine;
    //namespace mySynth = imajuscule::audio::sine;

    template<typename T>
    struct withChannels {
      withChannels(NoXFadeChans & chans) : chans(chans), obj(std::make_from_tuple<T>(buffers)) {}
      ~withChannels() {
        std::lock_guard<std::mutex> l(isUsed); // see 'Using'
      }

      template<typename Out>
      void onEvent2(Event e, Out & out) {
        obj.onEvent2(e, out, chans);
      }

      void finalize() {
        obj.finalize(chans);
      }

      T obj;

      // Note that if mononotechannel used pointers to the channels instead of ids
      // we would not need this field:
      NoXFadeChans & chans;

      std::mutex isUsed;

      static constexpr auto n_mnc = T::n_channels;
      using mnc_buffer = typename T::MonoNoteChannel::buffer_t;
      std::array<mnc_buffer,n_mnc> buffers;
    };

    // a 'Using' instance gives the guarantee that the object 'o' passed to its constructor
    // won't be destroyed during the entire lifetime of the instance, iff the following conditions hold:
    //   (1) T::~T() locks, then unlocks 'o.isUsed'
    //   (2) 'protectsDestruction' passed to the constructor is currently locked
    //          and 'o' cannot be destroyed until 'protectsDestruction' is unlocked
    template<typename T>
    struct Using {
      T & o; // this reference makes the object move-only, which is what we want

      Using(std::lock_guard<std::mutex> && protectsDestruction, T&o) : o(o) {
        o.isUsed.lock();
        // NOTE here, both the instrument lock (isUsed) and the 'protectsDestruction' lock
        // are taken.
        //
        // The order in which we take the locks is important to avoid deadlocks:
        // it is OK to take multiple locks at the same time, /only/ if, everywhere in the program,
        // we take them respecting a global order on the locks of the program.
        //
        // Hence, here the global order is:
        // map lock (protectsDestruction) -> instrument lock (isUsed)
      }
      ~Using() {
        o.isUsed.unlock();
      }
    };


    struct tryScopedLock {
      tryScopedLock(std::mutex&m) : m(m) {
        success = m.try_lock();
      }
      operator bool () const {
        return success;
      }
      ~tryScopedLock() {
        if(success) {
          m.unlock();
        }
      }
    private:
      std::mutex & m;
      bool success;
    };

    template <typename Envel>
    struct Synths {
      using T = mySynth::SynthT<Envel>;
      using K = typename Envel::Param;

      // NOTE the 'Using' is constructed while we hold the lock to the map.
      // Hence, while garbage collecting / recycling, if we take the map lock,
      // and if the instrument lock is not taken, we have the guarantee that
      // the instrument lock won't be taken until we release the map lock.
      static Using<withChannels<T>> get(K const & rawEnvelParam) {
        using namespace audioelement;

        auto envelParam = ClampParam<Envel>::clamp(rawEnvelParam);
        {
          // we use a global lock because we can concurrently modify and lookup the map.
          std::lock_guard<std::mutex> l(mutex());

          auto & synths = map();

          auto it = synths.find(envelParam);
          if(it != synths.end()) {
            return Using(std::move(l), *(it->second));
          }
          if(auto * p = recycleInstrument(synths, envelParam)) {
            return Using(std::move(l), *p);
          }
          auto p = std::make_unique<withChannels<T>>(addNoXfadeChannels(T::n_channels));
          SetParam<Envel>::set(envelParam, p->obj);
          if(!p->obj.initialize(p->chans)) {
            auto oneSynth = synths.begin();
            if(oneSynth != synths.end()) {
              LG(ERR, "a preexisting synth is returned");
              // The channels have the same lifecycle as the instrument, the instrument will be destroyed
              //  so we remove the associated channels:
              removeRecentNoXFadeChannels(&p->chans);
              return Using(std::move(l), *(oneSynth->second.get()));
            }
            LG(ERR, "an uninitialized synth is returned");
          }
          return Using(
              std::move(l)
            , *(synths.emplace(envelParam, std::move(p)).first->second));
        }
      }

      static void finalize() {
        std::lock_guard<std::mutex> l(mutex());
        for(auto & s : map()) {
          s.second->finalize();
        }
        map().clear();
      }

    private:
      using Map = std::map<K,std::unique_ptr<withChannels<T>>>;

      static auto & map() {
        static Map m;
        return m;
      }
      static auto & mutex() {
        static std::mutex m;
        return m;
      }

      /* The caller is expected to take the map mutex. */
      static withChannels<T> * recycleInstrument(Map & synths, K const & envelParam) {
        for(auto it = synths.begin(), end = synths.end(); it != end; ++it) {
          auto & i = it->second;
          if(!i) {
            LG(ERR,"inconsistent map");
            continue;
          }
          auto & o = *i;
          if(auto scoped = tryScopedLock(o.isUsed)) {
            // we don't take the audio lock because hasOrchestratorsOrComputes relies on an
            // atomically incremented / decremented counter.
            if(o.chans.hasOrchestratorsOrComputes()) {
              continue;
            }

            // - we have 0 orchestrator and 0 computes
            // - no note is being started, hence we won't increase the number of orchestrators or computes.
            // Hence, all enveloppes should be finished : if one is not finished, it will not have a chance to ever finish.
            Assert(o.obj.areEnvelopeFinished() && "inconsistent envelopes");

            // this code uses c++17 features not present in clang yet, so it's replaced by the code after.
            /*
            auto node = synths.extract(it);
            node.key() = envelParam;
            auto [inserted, isNew] = synths.insert(std::move(node));
            */
            std::unique_ptr<withChannels<T>> new_p;
            new_p.swap(it->second);
            synths.erase(it);
            auto [inserted, isNew] = synths.emplace(envelParam, std::move(new_p));

            Assert(isNew); // because prior to calling this function, we did a lookup
            using namespace audioelement;
            SetParam<Envel>::set(envelParam, inserted->second->obj);
            return inserted->second.get();
          }
          else {
            // a note is being started or stopped, we can't recycle this instrument.
          }
        }
        return nullptr;
      }

      static auto & addNoXfadeChannels(int nVoices) {
        static constexpr auto n_max_orchestrator_per_channel = 0; // we don't use orchestrators
        auto p = std::make_unique<NoXFadeChans>(
          getAudioContext().getChannelHandler().get_lock_policy(),
          std::min(nVoices, static_cast<int>(std::numeric_limits<uint8_t>::max())),
          n_max_orchestrator_per_channel);
        auto & res = *p;
        {
          auto & ch = getAudioContext().getChannelHandler();
          auto & cs = ch.getChannels().getChannelsNoXFade();
          auto w = mkVectorWrapper(cs);
          {
            NoXFadeChans::LockCtrlFromNRT l(ch.get_lock());
            reserveAndLock(1,w,l);
            cs.emplace_back(std::move(p));
            l.unlock();
          }
        }
        return res;
      }

      static void removeRecentNoXFadeChannels(NoXFadeChans * o) {
        auto & ch = getAudioContext().getChannelHandler();
        auto & cs = ch.getChannels().getChannelsNoXFade();
        std::unique_ptr<NoXFadeChans> p;
        {
          AudioFreeze l(ch.get_lock());
          extractFromEnd(cs,o).swap(p);
        }
        // deallocation happens outside the audio lock scope.
      }
    };

    template<typename Env>
    void midiEvent(typename Env::Param const & env, Event e) {
      Synths<Env>::get(env).o.onEvent2(e, getAudioContext().getChannelHandler());
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


// functions herein are part of the interface
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
