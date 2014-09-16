#ifndef _KERNEL_CPP_
#define _KERNEL_CPP_

#include "Kernel.h"
#include <iostream>

// int Kernel::get_free(int out_port)
// {
//     if (out_port > outputs.size()-1)
//         return 0;
//         
//     return outputs[out_port]->get_free();
// }
// 
// int Kernel::get_available(int in_port)
// {
//     int result = 0;
//     if (inputs.size() != 0)
//         result = inputs[in_port]->get_available();
//     return result;
// }
// 
// char * Kernel::read_value(int in_port)
// {
//     char *ptr = NULL;
//     int end_count = 0;
//     for(;;)
//     {
//         inputs[in_port]->read((char &) *ptr);
//         if(SPLIKELY(ptr != NULL))
//         {
//             clock.count += 1;
//             return ptr;
//         }
//     }
// }
// 
// bool Kernel::fireable()
// {
//     bool fireable = true;
//     //std::cout << "Checking fireable...";
//     //std::cout << " O" << outputs.size();
//     if (outputs.size() > 0)
//     {
//         fireable = fireable && outputs[0]->ready(outrate,true);
//     }
//     //std::cout << " I" << inputs.size();
//     if (inputs.size() > 0)
//     {
//         fireable = fireable && inputs[0]->ready(inrate,false);
//     }
//     //std::cout << std::endl;
//     return fireable;
// }
#endif