// This header is used only by test_wrapper.cpp

#pragma once

#include <cstdint>

#include "cpp.audio/include/public.h"

#include "extras.h"

extern "C" {

  bool initializeAudioOutput (float minLatencySeconds, int portaudioMinLatencyMillis);

  void teardownAudioOutput();

  void setMaxMIDIJitter(uint64_t v);

  bool midiNoteOnAHDSR_(imajuscule::audio::audioelement::OscillatorType osc,
                        imajuscule::audio::audioelement::EnvelopeRelease t,
                       int a, int ai, int h, int d, int di, float s, int r, int ri,
                       harmonicProperties_t * hars, int har_sz,
                       int16_t pitch, float velocity, int midiSource, uint64_t maybeMIDITime);
  bool midiNoteOffAHDSR_(imajuscule::audio::audioelement::OscillatorType osc,
                         imajuscule::audio::audioelement::EnvelopeRelease t,
                         int a, int ai, int h, int d, int di, float s, int r, int ri,
                         harmonicProperties_t * hars, int har_sz,
                         int16_t pitch, int midiSource, uint64_t maybeMIDITime);
  bool midiNoteOnAHDSRSweep_(imajuscule::audio::audioelement::OscillatorType osc,
                        imajuscule::audio::audioelement::EnvelopeRelease t,
                       int a, int ai, int h, int d, int di, float s, int r, int ri,
                       harmonicProperties_t * hars, int har_sz,
                       int sweep_duration,
                       float sweep_final_freq,
                       int16_t pitch, float velocity, int midiSource, uint64_t maybeMIDITime);
  bool midiNoteOffAHDSRSweep_(imajuscule::audio::audioelement::OscillatorType osc,
                         imajuscule::audio::audioelement::EnvelopeRelease t,
                         int a, int ai, int h, int d, int di, float s, int r, int ri,
                         harmonicProperties_t * hars, int har_sz,
                         int sweep_duration,
                         float sweep_final_freq,
                         int16_t pitch, int midiSource, uint64_t maybeMIDITime);
  double* analyzeAHDSREnvelope_(imajuscule::audio::audioelement::EnvelopeRelease t, int a, int ai, int h, int d, int di, float s, int r, int ri, int*nElems, int*splitAt);
  bool effectOn(int program, int16_t pitch, float velocity);
  bool effectOff(int16_t pitch);
  bool getConvolutionReverbSignature_(const char * dirPath, const char * filePath, spaceResponse_t * r);
  bool dontUseReverb_();
  bool useReverb_(const char * dirPath, const char * filePath);
  bool setReverbWetRatio(double wet);
}
