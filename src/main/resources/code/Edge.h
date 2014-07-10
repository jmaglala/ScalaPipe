#ifndef _EDGE_H_
#define _EDGE_H_

#include "ScalaPipe.h"
//#include "Kernel.h"

class Kernel;

class Edge
{
public:
    SPQ * queue;
    int depth;

    Kernel * source;
    Kernel * dest;

    Edge(int depth, Kernel * source, Kernel * dest, size_t width);
    ~Edge();
    int get_free();
    void * allocate();
    void send();
    int get_available();
    void * read_value();
    void release();
    void finish();
};

#endif // _EDGE_H_

