#include "Segment.h"
#include <iostream>

static int sid = 0;
Segment::Segment(int segId, std::vector<Kernel*> & kernels) : id(segId), kernelList(kernels) {
    //Build kernel list
    //this->kernelList = kernels;
    /*for (int i = 0; i < kernels->depth(); i++) {
        kernelList.push_back(kernels[i]);
    }*/
    
    state = 0;
    runtime = 0;
    output_rate = 1;
    
    //Set input rate, output rate, state, and runtime
    if (kernelList.front()->id != 0)
        input_rate = kernelList.front()->inrate;
    else
        input_rate = 0;
    for (int i = 0; i < kernelList.size(); i++) {
        if (i > 0) {
            output_rate /= kernelList[i]->inrate;
        }
        output_rate *= kernelList[i]->outrate;
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
    
    in_buf_size = 0;
    out_buf_size = 0;
    //Set input and output buffer sizes
    if (kernelList[0]->inputs.size() > 0) {
        in_buf_size = kernelList.front()->inputs.front()->depth;
    }
    if (kernelList.back()->outputs.size() > 0) {
        out_buf_size = kernelList.back()->outputs.front()->depth;
    }
    
    //Calculate the max times the segment can fire
    if (in_buf_size == 0)
        max_fires = out_buf_size/output_rate;
    else if (out_buf_size == 0)
        max_fires = in_buf_size/input_rate;
    else
        max_fires = std::min(in_buf_size/input_rate, out_buf_size/output_rate);
}

//Determine if segment is fireable
bool Segment::isFireable(int * segFireCount, bool firstSegOnThread, bool lastSegOnThread) {
    int segFireIterations = 0;
    /*std::cout << "in_buf_size: " << in_buf_size << std::endl;
    std::cout << "out_buf_size: " << out_buf_size << std::endl;
    std::cout << "input_rate: " << input_rate << std::endl;
    std::cout << "output_rate: " << output_rate << std::endl;
    std::cout << "out_avail: " << kernelList.back()->outputs[0]->get_available() << std::endl;*/
    //Get maximum times it can fire from the input
    int maxInputFires = 0;
    if (firstSegOnThread && in_buf_size > 0) {
        maxInputFires = kernelList.front()->get_available(0)/input_rate;
    }
    else if (in_buf_size > 0) {
        maxInputFires = segFireCount[id-1]/input_rate;
    }
    else
        maxInputFires = 0;
    //Get maximum times it can fire into the output
    int maxOutputFires = 0;
    if (lastSegOnThread && out_buf_size > 0) {
        maxOutputFires = (out_buf_size - kernelList.back()->outputs[0]->get_available())/output_rate;
    }
    else if (out_buf_size > 0) {
        maxOutputFires = (out_buf_size - segFireCount[id])/output_rate;
    }
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
    
    if (segFireIterations < max_fires * .5)
        return false;
    //std::cout << "isFireable seg iter =: " << "maxInputFires: " << maxInputFires << " maxOutputFires: " << maxOutputFires << std::endl;
    return true;
}

int Segment::fireIterations(int * segFireCount) {
    //If it's the first segment, return the amount of times the output can fire
    if (in_buf_size == 0) {
        //std::cout << "segId: " << id << std::endl;
        return (out_buf_size - segFireCount[id])/output_rate;
    }
    //If it's the last segment, return the amount of times the input can fire
    else if (out_buf_size == 0)
        return segFireCount[id-1]/input_rate;
    //Otherwise take their minimum
    else {
        //std::cout << "segId: " << id << std::endl;
        //std::cout << "fireIterations seg iter =: " << std::min(segFireCount[id-1]/input_rate, (out_buf_size - segFireCount[id])/output_rate) << std::endl;
        //std::cin.get();
        return std::min(segFireCount[id-1]/input_rate, (out_buf_size - segFireCount[id])/output_rate);
    }
}

void Segment::fire() {
    bool fired = false;
    bool done = false;
    int fireKernelNum = 0;
    //std::cout << "start fire func" << std::endl;
    while (done == false) {
        //If it's the first kernel
        if (fireKernelNum == 0) {
            //std::cout << "first kern" << std::endl;
            //std::cout << "avail: " << kernelList[fireKernelNum+1]->get_available(0) << std::endl;
            //std::cout << "outRate: " << kernelList[fireKernelNum]->outrate << std::endl;
            //std::cout << "depth: " << kernelList[fireKernelNum]->outputs.front()->depth << std::endl;
            //End if this segment has already fired
            if (fired == true) {
                //std::cout << "done firing" << std::endl;
                done = true;
            }
            //If the one fire of this kern + the next buffer's current size < its total size then fire
            else if (kernelList[fireKernelNum+1]->get_available(0) + kernelList[fireKernelNum]->outrate <= kernelList[fireKernelNum]->outputs.front()->depth) {
                //std::cout << "first kernel" << std::endl;
                kernelList[fireKernelNum]->run();
                fired = true;
                //If the next kernel can fire after this kernel just fired, move to it
                if (kernelList[fireKernelNum+1]->get_available(0) >= kernelList[fireKernelNum+1]->inrate) {
                    fireKernelNum++;
                    //std::cout << "moving on" << std::endl;
                }
            }
        }
        //If it's the last kernel
        else if (fireKernelNum == kernelList.size() - 1) {
            //std::cout << "Seg" << id << " last kernel" << std::endl;
            kernelList[fireKernelNum]->run();
            //If it doesn't have enough input for another fire, move back
            if (kernelList[fireKernelNum]->get_available(0) < kernelList[fireKernelNum]->inrate) {
                //std::cout << "moving back" << std::endl;
                fireKernelNum--;
            }
        }
        //If it's a middle kernel
        else {
            //std::cout << "kernel " << kernelList[fireKernelNum]->id << std::endl;
            /*std::cout << "avail: " << kernelList[fireKernelNum]->get_available(0) << std::endl;
            std::cout << "inrate: " << kernelList[fireKernelNum]->inrate << std::endl;
            std::cout << "outrate: " << kernelList[fireKernelNum]->outrate << std::endl;
            std::cout << "next avail: " << kernelList[fireKernelNum+1]->get_available(0) << std::endl;
            std::cout << "next inrate: " << kernelList[fireKernelNum+1]->inrate << std::endl;*/
            //If there is space in the output buffer for a fire and it has input,
            if (kernelList[fireKernelNum+1]->get_available(0) + kernelList[fireKernelNum]->outrate <= kernelList[fireKernelNum]->outputs.front()->depth &&
                kernelList[fireKernelNum]->get_available(0) >= kernelList[fireKernelNum]->inrate) {
                //std::cout << "firing" << std::endl;
                kernelList[fireKernelNum]->run();
                /*std::cout << "---------------------------------------" << std::endl;
                std::cout << "kernel " << fireKernelNum + 1 << std::endl;
                std::cout << "avail: " << kernelList[fireKernelNum]->get_available(0) << std::endl;
                std::cout << "inrate: " << kernelList[fireKernelNum]->inrate << std::endl;
                std::cout << "outrate: " << kernelList[fireKernelNum]->outrate << std::endl;
                std::cout << "next avail: " << kernelList[fireKernelNum+1]->get_available(0) << std::endl;
                std::cout << "next inrate: " << kernelList[fireKernelNum+1]->inrate << std::endl;*/
                //If the next kernel can fire after this kernel just fired, move to it
                if (kernelList[fireKernelNum+1]->get_available(0) >= kernelList[fireKernelNum+1]->inrate) {
                    //std::cout << "moving on" << std::endl;
                    fireKernelNum++;
                }
            }
            //If it doesn't have enough input for another fire, move back
            else if (kernelList[fireKernelNum]->get_available(0) <= kernelList[fireKernelNum]->inrate ||
                kernelList[fireKernelNum+1]->get_available(0) + kernelList[fireKernelNum]->outrate > kernelList[fireKernelNum]->outputs.front()->depth) {
                //std::cout << "moving back" << std::endl;
                fireKernelNum--;
            }
        }
        //std::cout << std::endl;
    }
}


