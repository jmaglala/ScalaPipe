#ifndef _EDGE_H_
#define _EDGE_H_

#ifndef ALIGN
#define ALIGN 64
#endif

#include "ScalaPipe.h"      

#include <stdint.h>

class Edge
{
public:
    
    uint64_t m_size;
    int * m_buff;
    bool m_initialized;
    
    //Kernel * source;
    //Kernel * dest;
    
    // New interface
    Edge(){}
    ~Edge(){}
    
    virtual int read () = 0;
    virtual void write(const int val) = 0;
    
    virtual uint64_t get_available() = 0;
    virtual bool ready(uint64_t change, bool writing) = 0;
    virtual bool full() = 0;
    virtual bool empty() = 0;
    
    void set_buff(int * buff = NULL);
    bool is_initd();
    uint64_t get_size();
    uint64_t get_free();
};

#endif // _EDGE_H_

