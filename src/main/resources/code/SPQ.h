#ifndef __SPQ_H__
#define __SPQ_H__

#ifndef ALIGN
#define ALIGN 64
#endif

#include <stdint.h>
#include <vector>
#include <iostream>

#include "Edge.h"

template<typename T>
class SPQ : public Edge<T>
{
protected:
    uint64_t m_read_pos;
    uint64_t m_write_pos;
    
    uint64_t m_count;
public:
    SPQ(uint64_t size);
    
    T read();
    void write(const T val);
    uint64_t get_available();
    bool ready(uint64_t change, bool writing);
    bool full();
    bool empty();
    
};

#endif // __SPQ_H__