//
//  FBQuadTreeNode.m
//  AnnotationClustering
//
//  Created by Filip Bec on 05/01/14.
//  Copyright (c) 2014 Infinum Ltd. All rights reserved.
//

#import "FBQuadTreeNode.h"

@implementation FBQuadTreeNode

- (id)init
{
    self = [super init];
    if (self) {
        self.count = 0;
        self.northEast = nil;
        self.northWest = nil;
        self.southEast = nil;
        self.southWest = nil;
        self.annotations = [[NSMutableArray alloc] initWithCapacity:kNodeCapacity];
    }
    return self;
}

- (id)initWithBoundingBox:(FBBoundingBox)box
{
    self = [self init];
    if (self) {
        self.boundingBox = box;
    }
    return self;
}

- (BOOL)isLeaf
{
    return self.northEast ? NO : YES;
}

- (void)subdivide
{
    self.northEast = [[FBQuadTreeNode alloc] init];
    self.northWest = [[FBQuadTreeNode alloc] init];
    self.southEast = [[FBQuadTreeNode alloc] init];
    self.southWest = [[FBQuadTreeNode alloc] init];
    
    FBBoundingBox box = self.boundingBox;
    CGFloat xMid = (box.xf + box.x0) / 2.0;
    CGFloat yMid = (box.yf + box.y0) / 2.0;
    
    self.northEast.boundingBox = FBBoundingBoxMake(xMid, box.y0, box.xf, yMid);
    self.northWest.boundingBox = FBBoundingBoxMake(box.x0, box.y0, xMid, yMid);
    self.southEast.boundingBox = FBBoundingBoxMake(xMid, yMid, box.xf, box.yf);
    self.southWest.boundingBox = FBBoundingBoxMake(box.x0, yMid, xMid, box.yf);
    
}

#pragma mark -
#pragma mark - Bounding box functions

FBBoundingBox FBBoundingBoxMake(CGFloat x0, CGFloat y0, CGFloat xf, CGFloat yf)
{
    FBBoundingBox box;
    box.x0 = x0;
    box.y0 = y0;
    box.xf = xf;
    box.yf = yf;
    return box;
}

FBBoundingBox FBBoundingBoxForCGRect(CGRect cgRect)
{
	CGFloat minX = CGRectGetMinX(cgRect);
	CGFloat maxX = CGRectGetMaxX(cgRect);
	
    CGFloat minY = CGRectGetMinY(cgRect);
    CGFloat maxY = CGRectGetMaxY(cgRect);
    
    return FBBoundingBoxMake(minX, minY, maxX, maxY);
}

CGRect FBCGRectForBoundingBox(FBBoundingBox boundingBox)
{
	CGRect r;
	r.origin.x = boundingBox.x0;
	r.origin.y = boundingBox.y0;
	r.size.width = fabs(boundingBox.xf - boundingBox.x0);
	r.size.height = fabs(boundingBox.yf - boundingBox.y0);
    
    return r;
}

BOOL FBBoundingBoxContainsPoint(FBBoundingBox box, id<FBPoint> point)
{
	CGPoint p = point.point;
    BOOL containsX = box.x0 <= p.x && p.y <= box.xf;
    BOOL containsY = box.y0 <= p.y && p.y <= box.yf;
    return containsX && containsY;
}

BOOL FBBoundingBoxIntersectsBoundingBox(FBBoundingBox box1, FBBoundingBox box2)
{
    return (box1.x0 <= box2.xf && box1.xf >= box2.x0 && box1.y0 <= box2.yf && box1.yf >= box2.y0);
}

@end
