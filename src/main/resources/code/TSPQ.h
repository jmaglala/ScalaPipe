/*
 */

#ifndef __TSPQ_H__
#define __TSPQ_H__

#ifndef ALIGN
#define ALIGN 64
#endif

#include <stdint.h>
#include <atomic>
#include <vector>
#include <iostream>
#include <thread>

#include "Edge.h"

template<typename T>
class TSPQ : public Edge<T>
{
protected:
    
    // Pointers
    std::atomic<uint64_t> m_read_pos;
    std::atomic<uint64_t> m_write_pos;
    
    // helper count
    std::atomic<uint64_t> m_count;
    
    
public:
    T read();
    void write(const T val);
    
    bool ready(uint64_t change, bool writing);
    bool full();
    bool empty();
    
    uint64_t get_available();

    TSPQ(uint64_t size);
    ~TSPQ();
};

#endif // __TSPQ_H__