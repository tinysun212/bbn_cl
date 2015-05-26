/*
                                ********

Copyright 1992 by BBN Systems and Technologies, A division of Bolt,
Beranek and Newman Inc.

Permission to use, copy, modify and distribute this software and its
documentation is hereby granted without fee, provided that the above
copyright notice and this permission appear in all copies and in
supporting documentation, and that the name Bolt, Beranek and Newman
Inc. not be used in advertising or publicity pertaining to distribution
of the software without specific, written prior permission.  In
addition, BBN makes no respresentation about the suitability of this
software for any purposes.  It is provided "AS IS" without express or
implied warranties including (but not limited to) all implied warranties
of merchantability and fitness.  In no event shall BBN be liable for any
special, indirect or consequential damages whatsoever resulting from
loss of use, data or profits, whether in an action of contract,
negligence or other tortuous action, arising out of or in connection
with the use or performance of this software.

                                ********
*/

/* A little 'parameterized' package for ringbuffering */

/* Make this the type of object you want to buffer: */

#define TYPE char

#define BAD_FLAG 0
#define GOOD_FLAG 1
#define TRUE 1
#define FALSE 0

#define empty_ring(ring) ((ring)->next_out == (ring)->next_in)

                                                              
typedef struct ring_buffer_header {                           
  TYPE * first_bad;                                           
  TYPE * next_in;                                             
  TYPE * next_out;                                            
  TYPE   data[1];                                             
} * ringbuff;                                                 
                                                              
scan_ringbuff(ring, obj)                                      
ringbuff ring;                                                
register TYPE obj;                                            
{                                                             
  register TYPE * p;                                          
  register TYPE * first_bad = ring->first_bad;                
  register TYPE * next_in = ring->next_in;                    
                                                              
  for(p = ring->next_out; ;)                               
    {                                                         
      if(p == first_bad) p = ring->data;                      
      if(p == next_in) return FALSE;                          
      if (obj == *p++) return TRUE;                           
    }                                                         
}                                                             
                                                              
ringbuff make_ringbuff(slots)                                 
int slots;                                                    
{                                                             
  register ringbuff handle;                                   
  register TYPE * data;                                       
                                                              
  handle = (ringbuff)                                         
    malloc((3 * sizeof(TYPE *)) + (slots * sizeof(TYPE)));    
  data = handle->data;                                        
  handle->first_bad = data + slots;                           
  handle->next_in = data;                                     
  handle->next_out = data;                                    
  return handle;                                              
}                                                             
                                                              
put_ring(ring, obj)                                           
register ringbuff ring;                                       
register TYPE obj;                                            
{ register TYPE * next_in;                                    
                                                              
  next_in = ring->next_in;		/* Cache */           
  *(next_in++) = obj;                                         
  if(next_in == ring->first_bad) next_in = ring->data;        
  if(next_in == ring->next_out)                               
    {                                                         
      next_in--;                                              
      if(next_in < ring->data) next_in = ring->first_bad - 1; 
      ring->next_in = next_in;	/* decache */                 
      return BAD_FLAG;                                        
    }                                                         
  ring->next_in = next_in;	/* decache */                 
  return GOOD_FLAG;                                           
}                                                             
                                                              
TYPE                                                          
get_ring(ring)                                                
register ringbuff ring;                                       
{                                                             
  TYPE obj;                                                   
  register TYPE * next_out;                                   
                                                              
  next_out = ring->next_out;	/* cache */                   
  if(next_out == ring->next_in) return (TYPE) BAD_FLAG;       
  obj = *(next_out++);                                        
  if(next_out == ring->first_bad) ring->next_out = ring->data;
  else ring->next_out = next_out;	/* decache */         
  return obj; 
}
 



