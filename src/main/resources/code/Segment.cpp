#include "Segment.h"

Segment::Segment(std::vector<Kernel*> & kernels) : kernelList(kernels) {
    //Build kernel list
    //this->kernelList = kernels;
    /*for (int i = 0; i < kernels->depth(); i++) {
        kernelList.push_back(kernels[i]);
    }*/
    
    //Set input rate, output rate, state, and runtime
    for (int i = 0; i < kernelList.size(); i++) {
        if (i > 0) {
            input_rate /= kernelList[i]->inrate;
        }
        if (i < kernelList.size() - 1) {
            output_rate *= kernelList[i]->outrate;
        }
        state += kernelList[i]->state;
        runtime += kernelList[i]->runtime;
    }
    
    //Set threshold
    float tempThreshold = 1;
    if (kernelList.size() > 1) {
        for (int i = 0; i < kernelList.size() - 1; i++) {
            float currentAmp = kernelList[i]->outrate/kernelList[i+1]->inrate;
            tempThreshold *= currentAmp;
            if (tempThreshold < 1 && tempThreshold < threshold) {
                threshold = (1/tempThreshold);
            }
        }
    }
    
    //Set input and output buffer sizes
    if (kernelList[0]->inputs.size() > 0) {
        in_buf_size = kernelList.front()->inputs.front()->depth;
    }
    if (kernelList.back()->outputs.size() > 0) {
        out_buf_size = kernelList.back()->outputs.front()->depth;
    }
    
    //Calculate the max times the segment can fire
    max_fires = std::min(in_buf_size/input_rate, out_buf_size/output_rate);
}

//Determine if segment is fireable
bool Segment::isFireable(int * segFireCount, bool firstSegOnThread, bool lastSegOnThread) {
    int segFireIterations = 0;
    
    //Get maximum times it can fire from the input
    int maxInputFires = 0;
    if (firstSegOnThread && in_buf_size > 0)
        maxInputFires = kernelList.front()->get_available(0)/input_rate;
    else if (in_buf_size > 0)
        maxInputFires = segFireCount[kernelList.front()->id]/input_rate;
    else
        maxInputFires = 0;
    
    //Get maximum times it can fire into the output
    int maxOutputFires = 0;
    if (lastSegOnThread && out_buf_size > 0)
        maxOutputFires = (out_buf_size - kernelList.back()->get_available(0))/output_rate;
    else if (out_buf_size > 0)
        maxOutputFires = (out_buf_size - segFireCount[kernelList.back()->id])/output_rate;
    else
        maxOutputFires = 0;
    
    //If it's the first segment use its maximum output fires
    if (in_buf_size == 0) {
        segFireIterations = maxOutputFires;
    }
    //If it's the last segment use its maximum input fires
    else if (out_buf_size == 0) {
        segFireIterations = maxInputFires;
    }
    //If it's a middle segment, take the minimum
    else {
        segFireIterations = std::min(maxInputFires, maxOutputFires);
    }
    
    if (segFireIterations == 0 || segFireIterations < max_fires * .5)
        return false;
    
    return true;
}

int Segment::fireIterations(int * segFireCount) {
    //If it's the first segment, return the amount of times the output can fire
    if (in_buf_size == 0)
        return (out_buf_size - segFireCount[kernelList.back()->id])/output_rate;
    //If it's the last segment, return the amount of times the input can fire
    else if (out_buf_size == 0)
        return segFireCount[kernelList.front()->id]/input_rate;
    //Otherwise take their minimum
    else
        return std::min(segFireCount[kernelList.front()->id]/input_rate, (out_buf_size - segFireCount[kernelList.back()->id])/output_rate);
}

void Segment::fire() {
    bool fired = false;
    bool done = false;
    int fireKernelNum = 0;
    while (done == false) {
        //If it's the first kernel
        if (fireKernelNum == 0) {
            //End if this segment has already fired
            if (fired == true)
                done = true;
            //If the one fire of this kern + the next buffer's current size < its total size then fire
            else if (kernelList[fireKernelNum+1]->get_available(0) + kernelList[fireKernelNum]->outrate < kernelList[fireKernelNum]->outputs.front()->depth) {
                kernelList[fireKernelNum]->run();
                fired = true;
                //If the next kernel can fire after this kernel just fired, move to it
                if (kernelList[fireKernelNum+1]->get_available(0) > kernelList[fireKernelNum+1]->inrate)
                    fireKernelNum++;
            }
        }
        //If it's the last kernel
        else if (fireKernelNum == kernelList.size() - 1) {
            kernelList[fireKernelNum]->run();
            //If it doesn't have enough input for another fire, move back
            if (kernelList[fireKernelNum]->get_available(0) < kernelList[fireKernelNum]->inrate)
                fireKernelNum--;
        }
        //If it's a middle kernel
        else {
            //If the one fire of this kern + the next buffer's current size < its total size then fire
            if (kernelList[fireKernelNum+1]->get_available(0) + kernelList[fireKernelNum]->outrate < kernelList[fireKernelNum]->outputs.front()->depth) {
                kernelList[fireKernelNum]->run();
            }
            //If the next kernel can fire after this kernel just fired, move to it
            if (kernelList[fireKernelNum+1]->get_available(0) > kernelList[fireKernelNum+1]->inrate)
                fireKernelNum++;
            //If it doesn't have enough input for another fire, move back
            else if (kernelList[fireKernelNum]->get_available(0) < kernelList[fireKernelNum]->inrate)
                fireKernelNum--;
        }
    }
}