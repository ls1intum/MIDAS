//
//  MPPacker.h
//  MessagePack
//
//  Created by Thomas Günzel on 20/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface MPPacker : NSObject

-(instancetype)init;

// Integer
-(BOOL)streamPackUInt8:(UInt8)i;
-(BOOL)streamPackUInt16:(UInt16)i;
-(BOOL)streamPackUInt32:(UInt32)i;
-(BOOL)streamPackUInt64:(UInt64)i;
-(BOOL)streamPackInt8:(SInt8)i;
-(BOOL)streamPackInt16:(SInt16)i;
-(BOOL)streamPackInt32:(SInt32)i;
-(BOOL)streamPackInt64:(SInt64)i;

// Float/Double
-(BOOL)streamPackFloat:(float)i;
-(BOOL)streamPackDouble:(double)i;

// Nil, True/False
-(BOOL)streamPackNil;
-(BOOL)streamPackTrue;
-(BOOL)streamPackFalse;

// Container (Map, Array)
-(BOOL)streamPackArray:(size_t)arraySize;
-(BOOL)streamPackMap:(size_t)mapSize;

// Bin/String
-(BOOL)streamPackBin:(size_t)binSize;
-(BOOL)streamPackBinBody:(const void*)body length:(size_t)l;
-(BOOL)streamPackStr:(size_t)strLength;
-(BOOL)streamPackStrBody:(const void*)body length:(size_t)l;

// Native Obj-C

-(BOOL)packArray:(NSArray*)array;
-(BOOL)packDictionary:(NSDictionary*)dictionary;
-(BOOL)packNumber:(NSNumber*)number;
-(BOOL)packString:(NSString*)string;
-(BOOL)packData:(NSData*)data;
-(BOOL)packObjCType:(const char*)type addr:(const void*)addr;
-(BOOL)pack:(id)object;

-(NSData*)data;

@end
