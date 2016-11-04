//
//  FBQuadTreeNode.h
//  AnnotationClustering
//
//  Created by Filip Bec on 05/01/14.
//  Copyright (c) 2014 Infinum Ltd. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreGraphics/CoreGraphics.h>
#import "FBPoint.h"

#define kNodeCapacity 8

typedef struct {
    CGFloat x0;
    CGFloat y0;
    CGFloat xf;
    CGFloat yf;
} FBBoundingBox;

FBBoundingBox FBBoundingBoxMake(CGFloat x0, CGFloat y0, CGFloat xf, CGFloat yf);

FBBoundingBox FBBoundingBoxForCGRect(CGRect cgRect);
CGRect FBCGRectForBoundingBox(FBBoundingBox boundingBox);

BOOL FBBoundingBoxContainsPoint(FBBoundingBox box, id<FBPoint> point);
BOOL FBBoundingBoxIntersectsBoundingBox(FBBoundingBox box1, FBBoundingBox box2);


/**
 Quad Tree Node. You should never use this class.
 */
@interface FBQuadTreeNode : NSObject

@property (nonatomic, assign) NSUInteger count;
@property (nonatomic, assign) FBBoundingBox boundingBox;

@property (nonatomic, strong) NSMutableArray *annotations;

@property (nonatomic, strong) FBQuadTreeNode *northEast;
@property (nonatomic, strong) FBQuadTreeNode *northWest;
@property (nonatomic, strong) FBQuadTreeNode *southEast;
@property (nonatomic, strong) FBQuadTreeNode *southWest;


/**
 Custom init method.
 @param box Bounding box of the node.
 @returns An instance of @c FBQuadTreeNode
 */
- (id)initWithBoundingBox:(FBBoundingBox)box;

/**
 Check if node is leaf in tree.
 @returns @c YES if node is leaf, else returns @c NO
 */
- (BOOL)isLeaf;

/**
 Create new child nodes.
 */
- (void)subdivide;

@end
