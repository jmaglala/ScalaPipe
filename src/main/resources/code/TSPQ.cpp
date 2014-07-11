#include "TSPQ.h"

/** Initialize the queue. */
TSPQ::TSPQ(uint32_t _depth, size_t _width)
{
    flags = 0;
    read_ptr.store(0);
    write_ptr.store(0);
    wrap_ptr.store(0);
    depth = _depth;
    width = _width;
    cookie = SPQ_COOKIE;
    data = (char*)malloc(depth*width);
}

TSPQ::~TSPQ()
{
    free(data);
}

/** Mark the queue as closed. */
void TSPQ::close()
{
    flags |= SPQ_FLAG_CLOSED;
}

/** Determine how many bytes are needed for the specified queue. */
size_t TSPQ::get_size(uint32_t depth, uint32_t width)
{
    return sizeof(SPQ) + depth * width;
}

/** Determine if the queue is valid. */
bool TSPQ::is_valid()
{
    return cookie == SPQ_COOKIE;
}

/** Determine if the queue has been closed. */
bool TSPQ::is_closed()
{
    return (flags & SPQ_FLAG_CLOSED) != 0;
}

/** Determine if the queue is empty. */
bool TSPQ::is_empty()
{
    
    auto read_ptr_l = read_ptr.load(std::memory_order_acquire);
    auto write_ptr_l = write_ptr.load(std::memory_order_acquire);
    auto wrap_ptr_l = wrap_ptr.load(std::memory_order_acquire);
    return (read_ptr_l == write_ptr_l) 
    || (write_ptr_l == 0 && read_ptr_l == wrap_ptr_l);
}

/** Determine how much space is available in the queue.
 * Assuming the thread pushing data on to the queue calls this,
 * the queue will have at least this much space free.
 * This will return the maximum size of a write that can be supported.
 * In other words, the buffer may have more free space than is returned.
 */
int TSPQ::get_free()
{
    /* Get a local copy of volatiles. */
    int read_ptr_l = (int)read_ptr.load(std::memory_order_acquire);
    int write_ptr_l = (int)write_ptr.load(std::memory_order_acquire);
    
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
int TSPQ::get_used()
{
    const int read_ptr_l = (int)read_ptr.load(std::memory_order_acquire);
    const int write_ptr_l = (int)write_ptr.load(std::memory_order_acquire);
    if(read_ptr_l <= write_ptr_l) {
        /* (free) read (data) write (free) */
        return write_ptr_l - read_ptr_l;
    } else {
        /* (data) write (free) read (data) wrap */
        return write_ptr_l + depth - read_ptr_l;
    }
}
int TSPQ::start_write_offset(uint32_t count)
{
    uint32_t wrap_needed = 0;
    uint32_t start;
    uint32_t end;
    uint32_t read_ptr_l;
    uint32_t wrap_ptr_l;
    
    start     = write_ptr.load(std::memory_order_acquire);
    end        = start + count;
    read_ptr_l = read_ptr.load(std::memory_order_acquire);
    wrap_ptr_l = wrap_ptr.load(std::memory_order_acquire);
    
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
        wrap_ptr.store(wrap_ptr_l,std::memory_order_release);
    if(start == 0) {
        write_ptr.store(0,std::memory_order_release);
    }
    
    // Return a pointer to a place to write.
    return start;
}
/** Get a buffer for writing.
 * Note that "count" can be no more than half the size of the queue.
 * This will return NULL if there is no room.
 */
char * TSPQ::start_write(uint32_t count)
{
    const int offset = start_write_offset(count);
    if (offset >= 0) {
        return &data[offset * width];
    } else {
        return NULL;
    }
}

/** Get a buffer for writing (blocking version). */
char * TSPQ::start_blocking_write(uint32_t count)
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
void TSPQ::finish_write(uint32_t count)
{
    int write_ptr_l = write_ptr.load(std::memory_order_acquire);
    write_ptr_l += count;
    write_ptr.store(write_ptr_l,std::memory_order_release);
}

/** Start a read.
 * This function does not block.  It returns the number of items available.
 * The offset parameter is in items.
 */
uint32_t TSPQ::start_read_offset(int * offset)
{
    for(;;) {
        
        // Get a copy of the pointers.
        const uint32_t read_ptr_l = read_ptr.load(std::memory_order_acquire);
        const uint32_t write_ptr_l = write_ptr.load(std::memory_order_acquire);
        const uint32_t wrap_ptr_l = wrap_ptr.load(std::memory_order_acquire);
        
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
                read_ptr.store(0,std::memory_order_release);
                
            } else {
                
                // Data from read_ptr to wrap_ptr.
                *offset = read_ptr_l;
                return wrap_ptr_l - read_ptr_l;
                
            }
        }
    }
}

/** Start a read. */
uint32_t TSPQ::start_read(char ** buffer)
{
    int offset = 0;
    const uint32_t count = start_read_offset(&offset);
    *buffer = &data[offset * width];
    return count;
}

/** Start a read (blocking version). */
uint32_t TSPQ::start_blocking_read(char ** buffer)
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
void TSPQ::finish_read(uint32_t count)
{
    int read_ptr_l = read_ptr.load(std::memory_order_acquire);
    read_ptr_l += count;
    read_ptr.store(read_ptr_l,std::memory_order_release);
}