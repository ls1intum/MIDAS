//
//  FBAnnotationCluster.m
//  AnnotationClustering
//
//  Created by Filip Bec on 06/01/14.
//  Copyright (c) 2014 Infinum Ltd. All rights reserved.
//

#import "FBAnnotationCluster.h"

@implementation FBAnnotationCluster

-(NSString *)description {
	return [NSString stringWithFormat:@"%@ count=%lu",[super description],self.annotations.count];
}

@end
