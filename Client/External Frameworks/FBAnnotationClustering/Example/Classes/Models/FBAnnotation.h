//
//  FBAnnotation.h
//  AnnotationClustering
//
//  Created by Filip Bec on 06/01/14.
//  Copyright (c) 2014 Infinum Ltd. All rights reserved.
//

#import <Foundation/Foundation.h>
@import MapKit;

// My custom annotation objects

@interface FBAnnotation : NSObject <MKAnnotation>

@property (nonatomic, assign) CLLocationCoordinate2D coordinate;

@end
