//
//  FBClusterManager.m
//  AnnotationClustering
//
//  Created by Filip Bec on 05/01/14.
//  Copyright (c) 2014 Infinum Ltd. All rights reserved.
//

#import "FBClusteringManager.h"
#import "FBQuadTree.h"

#import "FBPoint.h"

static NSString * const kFBClusteringManagerLockName = @"co.infinum.clusteringLock";

#pragma mark - Utility functions

/*NSInteger FBZoomScaleToZoomLevel(MKZoomScale scale)
{
    double totalTilesAtMaxZoom = MKMapSizeWorld.width / 256.0;
    NSInteger zoomLevelAtMaxZoom = log2(totalTilesAtMaxZoom);
    NSInteger zoomLevel = MAX(0, zoomLevelAtMaxZoom + floor(log2f(scale) + 0.5));
    
    return zoomLevel;
}*/

CGFloat FBCellSizeForZoomScale(CGFloat zoomScale)
{
    /*NSInteger zoomLevel = FBZoomScaleToZoomLevel(zoomScale);
    
    switch (zoomLevel) {
        case 13:
        case 14:
        case 15:
            return 64;
        case 16:
        case 17:
        case 18:
            return 32;
        case 19:
            return 16;
            
        default:
            return 88;
    }*/
	return 128;
}

#pragma mark - FBClusteringManager

@interface FBClusteringManager ()

@property (nonatomic, strong) FBQuadTree *tree;
@property (nonatomic, strong) NSRecursiveLock *lock;

@end


@implementation FBClusteringManager

- (id)init
{
    return [self initWithAnnotations:nil];
}

- (id)initWithAnnotations:(NSArray *)annotations
{
    self = [super init];
    if (self) {
        _lock = [NSRecursiveLock new];
        [self addAnnotations:annotations];
    }
    return self;
}

- (void)setAnnotations:(NSArray *)annotations
{
    self.tree = nil;
    [self addAnnotations:annotations];
}

- (void)addAnnotations:(NSArray *)annotations
{
    if (!self.tree) {
        self.tree = [[FBQuadTree alloc] init];
    }

    [self.lock lock];
    for (id<FBAnnotation> annotation in annotations) {
        [self.tree insertAnnotation:annotation];
    }
    [self.lock unlock];
}

- (void)removeAnnotations:(NSArray *)annotations
{
    if (!self.tree) {
        return;
    }

    [self.lock lock];
    for (id<FBAnnotation> annotation in annotations) {
        [self.tree removeAnnotation:annotation];
    }
    [self.lock unlock];
}

- (NSArray *)clusteredAnnotationsWithinCGRect:(CGRect)rect withZoomScale:(double)zoomScale
{
    return [self clusteredAnnotationsWithinCGRect:rect withZoomScale:zoomScale withFilter:nil];
}

- (NSArray *)clusteredAnnotationsWithinCGRect:(CGRect)rect withZoomScale:(double)zoomScale withFilter:(BOOL (^)(id<FBAnnotation>)) filter
{
    double cellSize = FBCellSizeForZoomScale(zoomScale);
    if ([self.delegate respondsToSelector:@selector(cellSizeFactorForCoordinator:)]) {
        cellSize *= [self.delegate cellSizeFactorForCoordinator:self];
    }
    double scaleFactor = zoomScale / cellSize;
    
    NSInteger minX = floor(CGRectGetMinX(rect) * scaleFactor);
    NSInteger maxX = floor(CGRectGetMaxX(rect) * scaleFactor);
    NSInteger minY = floor(CGRectGetMinY(rect) * scaleFactor);
    NSInteger maxY = floor(CGRectGetMaxY(rect) * scaleFactor);
    
    NSMutableArray *clusteredAnnotations = [[NSMutableArray alloc] init];
    
    [self.lock lock];
    for (NSInteger x = minX; x <= maxX; x++) {
        for (NSInteger y = minY; y <= maxY; y++) {
            CGRect mapRect = CGRectMake(x/scaleFactor, y/scaleFactor, 1.0/scaleFactor, 1.0/scaleFactor);
            FBBoundingBox mapBox = FBBoundingBoxForCGRect(mapRect);
            
            __block double totalX = 0;
            __block double totalY = 0;
            
            NSMutableArray *annotations = [[NSMutableArray alloc] init];

            [self.tree enumerateAnnotationsInBox:mapBox usingBlock:^(id<FBAnnotation> obj) {
                
                if(!filter || (filter(obj) == TRUE))
                {
					FBPoint *fbPoint = [obj fbPoint];
					CGPoint point = fbPoint.point;
                    totalX += point.x;
                    totalY += point.y;
                    [annotations addObject:obj];
                }
            }];
            
            NSInteger count = [annotations count];
            if (count == 1) {
                [clusteredAnnotations addObjectsFromArray:annotations];
            }
            
            if (count > 1) {
				FBPoint *p = [FBPoint pointWithX:(totalX/count) andY:(totalY/count)];
                FBAnnotationCluster *cluster = [[FBAnnotationCluster alloc] init];
                cluster.fbPoint = p;
                cluster.annotations = annotations;
                [clusteredAnnotations addObject:cluster];
            }
        }
    }
    [self.lock unlock];
    
    return [NSArray arrayWithArray:clusteredAnnotations];
}

- (NSArray *)allAnnotations
{
    NSMutableArray *annotations = [[NSMutableArray alloc] init];
    
    [self.lock lock];
    [self.tree enumerateAnnotationsUsingBlock:^(id<FBAnnotation> obj) {
        [annotations addObject:obj];
    }];
    [self.lock unlock];
    
    return annotations;
}

/*- (void)displayAnnotations:(NSArray *)annotations onMapView:(MKMapView *)mapView
{
    NSMutableSet *before = [NSMutableSet setWithArray:mapView.annotations];
    MKUserLocation *userLocation = [mapView userLocation];
    if (userLocation) {
        [before removeObject:userLocation];
    }
    NSSet *after = [NSSet setWithArray:annotations];
    
    NSMutableSet *toKeep = [NSMutableSet setWithSet:before];
    [toKeep intersectSet:after];
    
    NSMutableSet *toAdd = [NSMutableSet setWithSet:after];
    [toAdd minusSet:toKeep];
    
    NSMutableSet *toRemove = [NSMutableSet setWithSet:before];
    [toRemove minusSet:after];
    
    [[NSOperationQueue mainQueue] addOperationWithBlock:^{
        [mapView addAnnotations:[toAdd allObjects]];
        [mapView removeAnnotations:[toRemove allObjects]];
    }];
}*/

@end
