#include "Segment.h"
#include <iostream>

static int sid = 0;
Segment::Segment(int segId, std::vector<Kernel*> & kernels) : id(segId), kernelList(kernels) {    
    state = 0;
    runtime = 0;
    output_rate = 1;
    threshold = 1;
    in_buf_size = 0;
    out_buf_size = 0;
    read_count = 0;
    read_buf_fireable = false;
    read_count_threshold = 0;
    write_count = 0;
    write_buf_fireable = true;
    write_count_threshold = 0;
    
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
    
    
    //Set input and output buffer sizes
    if (kernelList[0]->inputs.size() > 0) {
        in_buf_size = kernelList.front()->inputs.front()->m_size;
    }
    if (kernelList.back()->outputs.size() > 0) {
        out_buf_size = kernelList.back()->outputs.front()->m_size;
    }
    
    //Calculate the max times the segment can fire
    //and set count thresholds to update adjacent segment's fireability
    if (in_buf_size == 0) {
        max_fires = out_buf_size/kernelList.back()->outrate;
        write_count_threshold = max_fires;
    }
    else if (out_buf_size == 0) {
        max_fires = in_buf_size/input_rate;
        read_count_threshold = max_fires;
    }
    else {
        write_count_threshold = out_buf_size/kernelList.back()->outrate;
        read_count_threshold = in_buf_size/input_rate;
        max_fires = std::min(in_buf_size/input_rate, out_buf_size/output_rate);
    }
    
    // Allocate memory
    allocate_memory();
    
    // Initialize the kernel states with something
    for (int i=0;i<kernelList.size();i++)
    {
        kernelList[i]->init();
    }
}

void Segment::allocate_memory()
{
    size_t size = 0;
    // Determine the size of the segment
    for (int i=0;i<kernelList.size();i++)
    {
        size += kernelList[i]->state * sizeof(uint8_t);
        if (i != kernelList.size()-1)
        {
            size += kernelList[i]->outputs[0]->get_size();
        }
    }
    
    // Ask for memory
    posix_memalign((void **)&buff,64,size);
    
    int addr = 0;
    
    // Notify everyone of their regions
    for (int i = 0;i<kernelList.size();i++)
    {
        kernelList[i]->set_state(&buff[addr]);
        addr += kernelList[i]->state * sizeof(uint8_t);
        if (i != kernelList.size() -1)
        {
            kernelList[i]->outputs[0]->set_buff((int *)&buff[addr]);
            addr += kernelList[i]->outputs[0]->get_size();
        }
    }
}

//Determine if segment is fireable
bool Segment::isFireable() {
    
    //If it's the first segment use its maximum output fires
    if (in_buf_size == 0) {
        return write_buf_fireable;
    }
    //If it's the last segment use its maximum input fires
    else if (out_buf_size == 0) {
        return read_buf_fireable;
    }
    //If it's a middle segment, take the minimum
    else {
        return write_buf_fireable && read_buf_fireable;
    }
}

int Segment::fireIterations(int * segFireCount) {
    //If it's the first segment, return the amount of times the output can fire
    if (in_buf_size == 0) {
        return (out_buf_size /output_rate);
    }
    // Otherwise, return the amount of times the input can fire
    else 
        return in_buf_size/input_rate;
}

void Segment::update_next_seg() {
    if (write_count == write_count_threshold) {
        write_count = 0;
    }
    else if (write_count == write_count_threshold / 2) {
        write_buf_fireable = false;
        next_seg->read_buf_fireable = true;
    }
}

void Segment::update_prev_seg() {
    if (read_count == read_count_threshold) {
        read_count = 0;
    }
    else if (read_count == (int)(read_count_threshold * .5)) {
        read_buf_fireable = false;
        prev_seg->write_buf_fireable = true;
    }
}

void Segment::fire() {
    
    //If there's only one kernel, fire it and return
    if (kernelList.size() == 1) {
        if (in_buf_size > 0) {
            read_count++;
            update_prev_seg();
        }
        kernelList[0]->run();
        if (out_buf_size > 0) {
            write_count++;
            update_next_seg();
        }
        return;
    }
    bool fired = false;
    bool done = false;
    int fireKernelNum = 0;
    
    while (done == false) 
    {
        //If it's the first kernel
        if (fireKernelNum == 0) {
            //End if this segment has already fired
            if (fired == true) {
                //std::cout << "done firing" << std::endl;
                done = true;
            }
            //If the one fire of this kern + the next buffer's current size < its total size then fire
            else if (kernelList[fireKernelNum+1]->get_available(0) + kernelList[fireKernelNum]->outrate <= kernelList[fireKernelNum]->outputs.front()->m_size) {
                kernelList[fireKernelNum]->run();
                if (in_buf_size > 0) {
                    read_count++;
                    update_prev_seg();
                }
                fired = true;
                //If the next kernel can fire after this kernel just fired, move to it
                if (kernelList[fireKernelNum+1]->get_available(0) >= kernelList[fireKernelNum+1]->inrate) {
                    fireKernelNum++;
                }
            }
        }
        //If it's the last kernel
        else if (fireKernelNum == kernelList.size() - 1) {
            kernelList[fireKernelNum]->run();
            if (out_buf_size > 0) {
                write_count++;
                update_next_seg();
            }
            //If it doesn't have enough input for another fire, move back
            if (kernelList[fireKernelNum]->get_available(0) < kernelList[fireKernelNum]->inrate) {
                fireKernelNum--;
            }
        }
        //If it's a middle kernel
        else {
            //If there is space in the output buffer for a fire and it has input,
            if (kernelList[fireKernelNum+1]->get_available(0) + kernelList[fireKernelNum]->outrate <= kernelList[fireKernelNum]->outputs.front()->m_size &&
                kernelList[fireKernelNum]->get_available(0) >= kernelList[fireKernelNum]->inrate) {
                kernelList[fireKernelNum]->run();

                //If the next kernel can fire after this kernel just fired, move to it
                if (kernelList[fireKernelNum+1]->get_available(0) >= kernelList[fireKernelNum+1]->inrate) {
                    fireKernelNum++;
                }
            }
            //If it doesn't have enough input for another fire, move back
            else if (kernelList[fireKernelNum]->get_available(0) <= kernelList[fireKernelNum]->inrate ||
                kernelList[fireKernelNum+1]->get_available(0) + kernelList[fireKernelNum]->outrate > kernelList[fireKernelNum]->outputs.front()->m_size) {
                fireKernelNum--;
            }
        }
    }
}




