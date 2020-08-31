#include "wrapper.h"

void play() {
    using namespace imajuscule;
    using namespace audio;
    using namespace audioelement;

    harmonicProperties_t har[10]{
        {0.f, 1.f},
        {0.f, 0.f},
        {0.f, 0.f},
        {0.f, 0.f},
        {0.f, 0.f},
        {0.f, 0.f},
        {0.f, 0.f},
        {0.f, 0.f},
        {0.f, 0.f},
        {0.f, 0.f},
    };
    
    midiNoteOnAHDSR_(OscillatorType::Sinus,
                     EnvelopeRelease::WaitForKeyRelease,
                     100,
                     27,
                     2560,
                     100,
                     4,
                     1,
                     12800,
                     22,
                     har,
                     10,
                     71,
                     1,
                     -1,
                     0);

    std::this_thread::sleep_for(std::chrono::milliseconds(300));

    midiNoteOffAHDSR_(OscillatorType::Sinus,
                     EnvelopeRelease::WaitForKeyRelease,
                     100,
                     27,
                     2560,
                     100,
                     4,
                     1,
                     12800,
                     22,
                     har,
                     10,
                     71,
                     -1,
                     0);
}

template<typename F>
void repeat(int n, F f) {
    for (int i=0; i<n; ++i) {
        f();
    }
}

int sequence() {
    // Reproduces the sequence of calls done by imj-game-synth when playing a note with reverb
    setMaxMIDIJitter(0);
    initializeAudioOutput(0.008f, 0);
    useReverb_("/Users/Olivier/Dev/audio.ir/waves/Theaters/Yerba Buena Theatre/", "Yerba_xog1v2.wir");
    
    repeat(10, play);
    
    std::this_thread::sleep_for(std::chrono::milliseconds(1000));

    teardownAudioOutput();
}
int main() {
    for(int i=0; i < 10; ++i) {
        std::cout << "iteration " << i << std::endl;
        sequence();
    }
    return 0;
}
