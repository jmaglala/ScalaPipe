#ifndef _EDGE_CPP_
#define _EDGE_CPP_

#include "Edge.h"

Edge::Edge(int _depth, Kernel * _source, Kernel * _dest, size_t width)
{
    source = _source;
    dest = _dest;
    depth = _depth + 1;
    queue = (SPQ*)malloc(spq_get_size(depth, width));
    spq_init(queue,depth,width);
}

Edge::~Edge()
{
    free(queue);
}

int Edge::get_free()
{
    return spq_get_free(queue);
}

void * Edge::allocate()
{
    return spq_start_write(queue,1);
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
    sp_decrement(&dest->active_inputs);
}

#endif // _EDGE_CPP_