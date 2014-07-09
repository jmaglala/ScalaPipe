#ifndef _EDGE_CPP_
#define _EDGE_CPP_

#include "Edge.h"
template<typename T>
Edge<T>::Edge(int _depth, Kernel * _source, Kernel * _dest)
{
    sourc = _source;
    dest = _dest;
    depth = _depth;
    queue = (SPQ*)malloc(spq_get_size(depth, sizeof(T)));
    sqp_init(queue,depth,sizeof(T))
}

Edge::~Edge()
{
    free(queue);
}

int Edge::get_free()
{
    return spq_get_free(queue);
}

void Edge::alocate()
{
    spq_start_write(queue,1);
}

void Edge::send()
{
    spq_finish_write(queue,1);
}

int Edge::get_available()
{
    return spq_get_used(queue);
}

void * Edge::read_value()
{
    char *buffer = NULL;
    if (spq_start_read(queue, &buffer) > 0)
        return buffer;
    return NULL;
}
void Edge::release()
{
    spq_finish_read(queue,1);
}

void Edge::finish()
{
    sp_decrement(&dest.active_inputs)
}

#endif // _EDGE_CPP_