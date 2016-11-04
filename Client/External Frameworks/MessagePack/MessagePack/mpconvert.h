//
//  mpconvert.h
//  MessagePack
//
//  Created by Thomas Günzel on 20/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#ifndef mpconvert_h
#define mpconvert_h

#import <Foundation/Foundation.h>
#import "msgpack.h"

id mp_convert(msgpack_object *object);
id mp_convert_mapped(msgpack_object *object, NSDictionary *class_mapping);

#endif /* mpconvert_h */
