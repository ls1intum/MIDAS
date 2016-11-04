//
//  FBQuadTree.m
//  AnnotationClustering
//
//  Created by Filip Bec on 05/01/14.
//  Copyright (c) 2014 Infinum Ltd. All rights reserved.
//

#import "FBQuadTree.h"

@import UIKit;

@implementation FBQuadTree

+(CGRect)rootRect {
	CGRect screen = [[UIScreen mainScreen] bounds];
	return screen;
}

- (id)init
{
    self = [super init];
    if (self) {
		CGRect screen = [FBQuadTree rootRect];
        self.rootNode = [[FBQuadTreeNode alloc] initWithBoundingBox:FBBoundingBoxForCGRect(screen)];
    }
    return self;
}

- (BOOL)insertAnnotation:(id<FBAnnotation>)annotation
{
    return [self insertAnnotation:annotation toNode:self.rootNode];
}

- (BOOL)removeAnnotation:(id<FBAnnotation>)annotation
{
    return [self removeAnnotation:annotation fromNode:self.rootNode];
}

- (BOOL)removeAnnotation:(id<FBAnnotation>)annotation fromNode:(FBQuadTreeNode *)node
{
    if (!FBBoundingBoxContainsPoint(node.boundingBox, annotation.fbPoint)) {
        return NO;
    }

    if ([node.annotations containsObject:annotation]) {
        [node.annotations removeObject:annotation];
        node.count--;
        return YES;
    }

    if ([self removeAnnotation:annotation fromNode:node.northEast]) return YES;
    if ([self removeAnnotation:annotation fromNode:node.northWest]) return YES;
    if ([self removeAnnotation:annotation fromNode:node.southEast]) return YES;
    if ([self removeAnnotation:annotation fromNode:node.southWest]) return YES;

    return NO;
}


- (BOOL)insertAnnotation:(id<FBAnnotation>)annotation toNode:(FBQuadTreeNode *)node
{
    if (!FBBoundingBoxContainsPoint(node.boundingBox, [annotation fbPoint])) {
        return NO;
    }
    
    if (node.count < kNodeCapacity) {
        node.annotations[node.count++] = annotation;
        return YES;
    }
    
    if ([node isLeaf]) {
        [node subdivide];
    }
    
    if ([self insertAnnotation:annotation toNode:node.northEast]) return YES;
    if ([self insertAnnotation:annotation toNode:node.northWest]) return YES;
    if ([self insertAnnotation:annotation toNode:node.southEast]) return YES;
    if ([self insertAnnotation:annotation toNode:node.southWest]) return YES;
    
    return NO;
}

- (void)enumerateAnnotationsInBox:(FBBoundingBox)box usingBlock:(void (^)(id<FBAnnotation>))block
{
    [self enumerateAnnotationsInBox:box withNode:self.rootNode usingBlock:block];
}

- (void)enumerateAnnotationsUsingBlock:(void (^)(id<FBAnnotation>))block
{
    [self enumerateAnnotationsInBox:FBBoundingBoxForCGRect([FBQuadTree rootRect]) withNode:self.rootNode usingBlock:block];
}

- (void)enumerateAnnotationsInBox:(FBBoundingBox)box withNode:(FBQuadTreeNode*)node usingBlock:(void (^)(id<FBAnnotation>))block
{
    if (!FBBoundingBoxIntersectsBoundingBox(node.boundingBox, box)) {
        return;
    }
    
    NSArray *tempArray = [node.annotations copy];
    
    for (id<FBAnnotation> annotation in tempArray) {
        if (FBBoundingBoxContainsPoint(box, [annotation fbPoint])) {
            block(annotation);
        }
    }
    
    if ([node isLeaf]) {
        return;
    }
    
    [self enumerateAnnotationsInBox:box withNode:node.northEast usingBlock:block];
    [self enumerateAnnotationsInBox:box withNode:node.northWest usingBlock:block];
    [self enumerateAnnotationsInBox:box withNode:node.southEast usingBlock:block];
    [self enumerateAnnotationsInBox:box withNode:node.southWest usingBlock:block];
}

@end
