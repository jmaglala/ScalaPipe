#ifndef __TSPQ_H__
#define __TSPQ_H__

#include <stdint.h>
#include <atomic>
#include "SPQ.h"

class TSPQ : public SPQ
{
    std::atomic<uint32_t> read_ptr;
    std::atomic<uint32_t> write_ptr;
    std::atomic<uint32_t> wrap_ptr;
    
public:
    TSPQ(uint32_t depth, uint32_t width);
    ~TSPQ();
    void close();
    size_t get_size(uint32_t depth, uint32_t width);
    bool is_valid();
    bool is_closed();
    bool is_empty();
    int get_free();
    int get_used();
    int start_write_offset(uint32_t count);
    char * start_write(uint32_t count);
    char * start_blocking_write(uint32_t count);
    void finish_write(uint32_t count);
    uint32_t start_read_offset(int * offset);
    uint32_t start_read(char ** buffer);
    uint32_t start_blocking_read(char ** buffer);
    void finish_read(uint32_t count);
};


#endif // __TSPQ_H__
