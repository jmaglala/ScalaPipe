#ifndef _EDGE_H_
#define _EDGE_H_

#ifndef ALIGN
#define ALIGN 64
#endif

#include "ScalaPipe.h"      

#include <stdint.h>

template <typename T>
class Edge
{
public:
    
    uint64_t m_size;
    T m_buff;
    bool m_initialized;
    
    Edge(){}
    ~Edge(){}
    
    virtual int read () = 0;
    virtual void write(const int val) = 0;
    
    virtual uint64_t get_available() = 0;
    virtual bool ready(uint64_t change, bool writing) = 0;
    virtual bool full() = 0;
    virtual bool empty() = 0;
    
    void set_buff(T buff = NULL);
    bool is_initd();
    
    // Size in bytes
    size_t get_size();
    uint64_t get_free();
    
    // Queue depth
    uint64_t size();
};

#endif // _EDGE_H_

