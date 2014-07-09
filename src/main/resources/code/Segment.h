#ifndef _SEGMENT_H_
#define _SEGMENT_H_

#include <vector>
#include "Kernel.h"
// Helper struct for storing information about a segment
class Segment
{
public:
    std::vector<Kernel> * kernelList;         // The list of mod_ids
    
    int id;              // An id from the scheudle
    
    float input_rate = 1;
    float output_rate = 1;
    float amplification = 1;
    int runtime = 0;
    int state = 0;
    int threshold = 1;
    
    Segment *        next_seg;            // The next segment
    Segment *        prev_seg;            // The previous segment
    
    uint8_t *        buff;
    
    Segment(std::vector<Kernel> * kernels);
    bool isFireable();
    int fireIterations();
    void fire();
};

#endif // _SEGMENT_H_