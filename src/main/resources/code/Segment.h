// Helper struct for storing information about a segment
class Segment
{
public:
    int kernels[];         // The list of mod_ids
    
    int id;              // An id from the scheudle
    int tid;            // The processor assigned
    
    float input_rate = 1;
    float output_rate = 1;
    float amplification = 1;
    int runtime = 0;
    int state = 0;
    int threshold = 1;
    
    Segment *        next_seg;            // The next segment
    Segment *        prev_seg;            // The previous segment
    
    uint8_t *        buff;
    
    Segment(int startKern, int endKern);
    void fire();
};