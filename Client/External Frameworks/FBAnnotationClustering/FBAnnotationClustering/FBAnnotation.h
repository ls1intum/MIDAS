//
//  FBAnnotation.h
//  FBAnnotationClustering
//
//  Created by Thomas Günzel on 15/08/16.
//  Copyright © 2016 Infinum Ltd. All rights reserved.
//

#import <Foundation/Foundation.h>

#import "FBPoint.h"

@protocol FBAnnotation <NSObject>

@property(readwrite,nonatomic,strong) id<FBPoint> fbPoint;


@end
