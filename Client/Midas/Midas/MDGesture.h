//
//  MDGesture.h
//  Midas
//
//  Created by Thomas Günzel on 17/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

@import UIKit;

#import "MDCoding.h"

@class MDTouchSequence;
@class MDRecordedGestures;

@interface MDGesture : NSObject<MDCoding>

-(instancetype)initWithTimeOffset:(NSTimeInterval)timeOffset;

// return NO, if this gesture ended (no touches active anymore)
-(BOOL)handleTouches:(NSSet<UITouch*> *)touches;

@property(readonly,nonatomic) NSTimeInterval timeOffset;

// Downloaded Only!
@property(readwrite,nonatomic) MDRecordedGestures *parent;
@property(readonly,nonatomic) NSArray<MDTouchSequence*> *touchSequences;
@property(readonly,nonatomic,strong) NSDictionary *contexts;

-(NSDictionary*)context:(NSString*)contextName;
@end
