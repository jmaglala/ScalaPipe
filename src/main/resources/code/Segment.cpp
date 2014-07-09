#include "Segment.h"

Segment::Segment(std::vector<Kernel> * kernels) {
    //Build kernel list
    for (int i = 0; i < kernels.length(); i++) {
        kernelList.push_back(kernels[i];
    }
    
    //Set input rate, output rate, state, and runtime
    for (int i = 0; i < kernelList.length(); i++) {
        if (i > 0) {
            input_rate /= kernelList[i].inrate;
        }
        if (i < kernelList.length() - 1) {
            output_rate */ kernelList[i].outrate;
        }
        state += kernelList[i].state;
        runtime += kernelList[i].runtime;
    }
    
    //Set threshold
    float tempThreshold = 1;
    if (kernelList.length() > 1) {
        for (int i = 0; i < kernelList.length() - 1; i++) {
            float currentAmp = kernelList(i).outrate/kernelList(i+1).inrate;
            tempThreshold *= currentAmp;
            if (tempThreshold < 1 && tempThreshold < threshold) {
                threshold = (1/tempThreshold);
            }
        }
    }
    
}

bool isFireable() {
    
}

int fireIterations() {
    
}

void Segment::fire() {
    
}