#ifndef _EDGE_H_
#define _EDGE_H_

#include "ScalaPipe.h"
//#include "Kernel.h"

class Kernel;

class EdgeBase
{
public:
    SPQ * queue;

    Kernel * source;
    Kernel * dest;
    
    int size;
    int get_free();
    void * allocate();
    void send();
    int get_available();
    void * read_value();
    void release();
    void finish();
};

template <typename T>
class Edge : public EdgeBase
{
public:

    Edge(int depth, Kernel * source, Kernel * dest);
    ~Edge();
    
};

#endif // _EDGE_H_

