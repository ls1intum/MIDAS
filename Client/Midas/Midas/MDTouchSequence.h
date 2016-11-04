//
//  MDTouchSequence.h
//  Midas
//
//  Created by Thomas Günzel on 17/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

@import UIKit;

#import "MDCoding.h"

@class MDGesture;

@interface MDTouchSequence : NSObject<MDCoding>

-(instancetype)initWithTimeOffset:(NSTimeInterval)timeOffset;

-(BOOL)handleTouch:(UITouch*)touch;

@property(readonly,nonatomic) NSTimeInterval timeOffset;


// Download only!
@property(readwrite,nonatomic) MDGesture *parent;
@property(readwrite,nonatomic,strong) NSMutableArray *touches;
-(NSDictionary*)context:(NSString*)contextName;
@end
