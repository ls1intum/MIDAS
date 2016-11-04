//
//  MDCoding.h
//  Midas
//
//  Created by Thomas Günzel on 16/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

@import Foundation;

#import "MDEncoder.h"
#import "MDDecoder.h"

@protocol MDCoding <NSObject>

-(instancetype)initFromDictionary:(NSDictionary*)dict;

//-(instancetype)initWithDecoder:(MDDecoder*)decoder;
-(void)encodeWithEncoder:(MDEncoder*)encoder;

@end
