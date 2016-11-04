//
//  FBPoint.h
//  FBAnnotationClustering
//
//  Created by Thomas Günzel on 15/08/16.
//  Copyright © 2016 Infinum Ltd. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreGraphics/CoreGraphics.h>

@protocol FBPoint <NSObject>

@property(readwrite,nonatomic) CGPoint point;

@end

@interface FBPoint : NSObject<FBPoint>

-(instancetype)initWithPoint:(CGPoint)p;
-(instancetype)initWithX:(CGFloat)x andY:(CGFloat)y;

+(instancetype)point:(CGPoint)p;
+(instancetype)pointWithX:(CGFloat)x andY:(CGFloat)y;

@property(readwrite,nonatomic) CGPoint point;

@end
