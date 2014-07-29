#ifndef _SEGMENT_H_
#define _SEGMENT_H_

#include <vector>
#include "Kernel.h"
// Helper struct for storing information about a segment
class Segment
{
    
public:
    void update_next_seg();
    void update_prev_seg();
    
    std::vector<Kernel*> kernelList;         // The list of mod_ids
    int * segFireCount;
    
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
    
    int read_count;
    bool read_buf_fireable;
    int read_count_threshold;
    int write_count;
    bool write_buf_fireable;
    int write_count_threshold;
    
    Segment *        next_seg;            // The next segment
    Segment *        prev_seg;            // The previous segment
    
    uint8_t *        buff;
    

    Segment(int ,std::vector<Kernel*> &, int *);
    bool isFireable();
    int fireIterations();
    void fire();
    void load();

private:
    void allocate_memory();     // Allocate memory for the kernel states and internal edges
};

#endif // _SEGMENT_H_