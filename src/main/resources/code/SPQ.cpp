#include "SPQ.h"

/** Initialize the queue. */
SPQ::SPQ(uint32_t _depth, size_t _width)
{
    flags = 0;
    read_ptr = 0;
    write_ptr = 0;
    wrap_ptr = 0;
    depth = _depth;
    width = _width;
    cookie = SPQ_COOKIE;
    data = (char*)malloc(depth*width);
}

SPQ::~SPQ()
{
    free(data);
}
/** Mark the queue as closed. */
void SPQ::close()
{
    flags |= SPQ_FLAG_CLOSED;
}

/** Determine how many bytes are needed for the specified queue. */
size_t SPQ::get_size(uint32_t depth, uint32_t width)
{
    return sizeof(SPQ) + depth * width;
}

/** Determine if the queue is valid. */
bool SPQ::is_valid()
{
    return cookie == SPQ_COOKIE;
}

/** Determine if the queue has been closed. */
bool SPQ::is_closed()
{
    return (flags & SPQ_FLAG_CLOSED) != 0;
}

/** Determine if the queue is empty. */
bool SPQ::is_empty()
{
    return (read_ptr == write_ptr) 
        || (write_ptr == 0 && read_ptr == wrap_ptr);
}

/** Determine how much space is available in the queue.
 * Assuming the thread pushing data on to the queue calls this,
 * the queue will have at least this much space free.
 * This will return the maximum size of a write that can be supported.
 * In other words, the buffer may have more free space than is returned.
 */
int SPQ::get_free()
{
    /* Get a local copy of volatiles. */
    const int read_ptr_l = (int)read_ptr;
    const int write_ptr_l = (int)write_ptr;
    
    if(read_ptr_l <= write_ptr_l) {
        
        /* (free) read (data) write (free) */
        /* Return the largest contiguous segement. */
        const int end_space = depth - write_ptr_l;
        const int begin_space = read_ptr_l - 1;
        if(end_space > begin_space) {
            return end_space;
        } else {
            return begin_space;
        }
        
    } else { /* read_ptr > write_ptr */
        
        /* (data) write (free) read (data/wrap) */
        return read_ptr_l - write_ptr_l - 1;
        
    }
}

/** Determine how much of a queue is used.
 * This assumes that the queue will not change size while running.
 */
int SPQ::get_used()
{
    const int read_ptr_l = (int)read_ptr;
    const int write_ptr_l = (int)write_ptr;
    if(read_ptr_l <= write_ptr_l) {
        /* (free) read (data) write (free) */
        return write_ptr_l - read_ptr_l;
    } else {
        /* (data) write (free) read (data) wrap */
        return write_ptr_l + depth - read_ptr_l;
    }
}
int SPQ::start_write_offset(uint32_t count)
{
    uint32_t wrap_needed = 0;
    uint32_t start;
    uint32_t end;
    uint32_t read_ptr_l;
    uint32_t wrap_ptr_l;
    
    start     = write_ptr;
    end        = start + count;
    read_ptr_l = read_ptr;
    wrap_ptr_l = wrap_ptr;
    
    if(end > depth) {
        
        // New end is past the end of the queue.
        
        // Make sure there is room.
        if(read_ptr_l <= count) {
            return -1;
        }
        end = count;
        wrap_ptr_l = start;
        start = 0;
        wrap_needed = 1;
        
    } else {
        
        // No wrap needed.
        
        // Update the wrap pointer if needed.
        if(end > wrap_ptr_l) {
            wrap_ptr_l = end;
        }
        
    }
    
    // Make sure there is enough space available.
    if(    (start < read_ptr_l && read_ptr_l <= end)
        || read_ptr_l > wrap_ptr_l
        || (wrap_needed && read_ptr_l <= end)) {
        
        return -1;
        }
        
        // We have room.
        // Update the write and wrap pointers.
        wrap_ptr = wrap_ptr_l;
    if(start == 0) {
        write_ptr = 0;
    }
    
    // Return a pointer to a place to write.
    return start;
}
/** Get a buffer for writing.
 * Note that "count" can be no more than half the size of the queue.
 * This will return NULL if there is no room.
 */
char * SPQ::start_write(uint32_t count)
{
    const int offset = start_write_offset(count);
    if (offset >= 0) {
        return &data[offset * width];
    } else {
        return NULL;
    }
}

/** Get a buffer for writing (blocking version). */
char * SPQ::start_blocking_write(uint32_t count)
{
    for(;;) {
        char *ptr = start_write(count);
        if(ptr != NULL) {
            return ptr;
        }
        sched_yield();
    }
}

/** Finish a write. */
void SPQ::finish_write(uint32_t count)
{
   write_ptr += count;
}

/** Start a read.
 * This function does not block.  It returns the number of items available.
 * The offset parameter is in items.
 */
uint32_t SPQ::start_read_offset(int * offset)
{
    for(;;) {
        
        // Get a copy of the pointers.
        const uint32_t read_ptr_l = read_ptr;
        const uint32_t write_ptr_l = write_ptr;
        const uint32_t wrap_ptr_l = wrap_ptr;
        
        if(read_ptr_l == write_ptr_l) {
            
            // Queue is empty.
            return 0;
            
        } else if(read_ptr_l < write_ptr_l) {
            
            // Data available from the read pointer to the write pointer.
            *offset = read_ptr_l;
            return write_ptr_l - read_ptr_l;
            
        } else { // read_ptr >= write_ptr

            if(read_ptr_l == wrap_ptr_l) {
                
                // Wrapped around.
                read_ptr = 0;
                
            } else {
                
                // Data from read_ptr to wrap_ptr.
                *offset = read_ptr_l;
                return wrap_ptr_l - read_ptr_l;
                
            }
        }
    }
}

/** Start a read. */
uint32_t SPQ::start_read(char ** buffer)
{
    int offset = 0;
    const uint32_t count = start_read_offset(&offset);
    *buffer = &data[offset * width];
    return count;
}

/** Start a read (blocking version). */
uint32_t SPQ::start_blocking_read(char ** buffer)
{
    for(;;) {
        const uint32_t rc = start_read(buffer);
        if(rc > 0) {
            return rc;
        }
        sched_yield();
    }
}

/** Finish a read. */
void SPQ::finish_read(uint32_t count)
{
    read_ptr += count;
}