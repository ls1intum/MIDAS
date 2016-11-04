//
//  MDTouch.h
//  Midas
//
//  Created by Thomas Günzel on 16/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

@import Foundation;
@import CoreGraphics;
@import FBAnnotationClustering;

#import "MDCoding.h"

@interface MDTouch : NSObject<MDCoding,FBPoint,FBAnnotation>

-(instancetype)initWithLocation:(CGPoint)location timeOffset:(NSTimeInterval)timeOffset;

@property(readonly,nonatomic) CGPoint location;
@property(readonly,nonatomic) NSTimeInterval timeOffset;


@property(readonly,nonatomic) CGPoint point;
@property(readonly,nonatomic) id<FBPoint> fbPoint;

@end
