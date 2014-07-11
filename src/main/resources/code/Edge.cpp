#ifndef _EDGE_CPP_
#define _EDGE_CPP_

#include "Edge.h"

Edge::Edge(int _depth, Kernel * _source, Kernel * _dest, size_t width) : queue(new TSPQ(_depth + 1, width))
{
    source = _source;
    dest = _dest;
    depth = _depth + 1;
    //queue = (SPQ*)malloc(spq_get_size(depth, width));
    //spq_init(queue,depth,width);
}

Edge::~Edge()
{
    free(queue);
}

int Edge::get_free()
{
    return queue->get_free();
}

void * Edge::allocate()
{
    return queue->start_write(1);
}

void Edge::send()
{
    queue->finish_write(1);
}

int Edge::get_available()
{
    return queue->get_used();
}

void * Edge::read_value()
{
    char *buffer = NULL;
    if (queue->start_read(&buffer) > 0)
        return buffer;
    return NULL;
}
void Edge::release()
{
    queue->finish_read(1);
}

void Edge::finish()
{
    //sp_decrement(&dest->active_inputs);
}

#endif // _EDGE_CPP_