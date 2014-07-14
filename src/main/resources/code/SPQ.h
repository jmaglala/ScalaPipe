#ifndef __SPQ_H__
#define __SPQ_H__

#define SPQ_COOKIE 0x1337
#define SPQ_FLAG_CLOSED    (1 << 0)

#include <stdint.h>

class SPQ
{
public:
    uint16_t cookie;
    uint16_t flags;
    uint8_t  pad0[4];
    
    volatile uint32_t read_ptr;
    uint8_t pad1[4];
    
    volatile uint32_t write_ptr;
    uint8_t pad2[4];
    
    volatile uint32_t wrap_ptr;
    uint8_t pad3[4];
    
    uint32_t depth;     /**< Number of items that can be put in the queue. */
    size_t width;     /**< Number of bytes for each element. */
    
    uint8_t pad4[8];    /**< Make this structure 16-byte aligned. */
    char * data;
    
    SPQ(){}
    
    SPQ(uint32_t depth, size_t width);
    ~SPQ();
    
    void set_buff(char * buff);
    
    virtual void close();
    virtual size_t get_size();
    virtual bool is_valid();
    virtual bool is_closed();
    virtual bool is_empty();
    virtual int get_free();
    virtual int get_used();
    virtual int start_write_offset(uint32_t count);
    virtual char * start_write(uint32_t count);
    virtual char * start_blocking_write(uint32_t count);
    virtual void finish_write(uint32_t count);
    virtual uint32_t start_read_offset(int * offset);
    virtual uint32_t start_read(char ** buffer);
    virtual uint32_t start_blocking_read(char ** buffer);
    virtual void finish_read(uint32_t count);
};

#endif // __SPQ_H__