#ifndef _SEGMENT_H_
#define _SEGMENT_H_

#include <vector>
#include "Kernel.h"
// Helper struct for storing information about a segment
class Segment
{
public:
    std::vector<Kernel*> kernelList;         // The list of mod_ids
    
    int id;              // An id from the scheudle
    
    float input_rate;
    float output_rate;
    float amplification;
    int runtime;
    int state;
    int threshold;
    int in_buf_size;
    int out_buf_size;
    int max_fires;
    
    Segment *        next_seg;            // The next segment
    Segment *        prev_seg;            // The previous segment
    
    uint8_t *        buff;
    
    Segment(std::vector<Kernel*> kernels);
    bool isFireable();
    int fireIterations();
    void fire();
};

#endif // _SEGMENT_H_