//
//  MDFunnel.h
//  Midas
//
//  Created by Thomas Günzel on 09/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

@class MDFunnel;
@class MDFilter;

typedef enum : NSUInteger {
	MDVisualizationTypeHeatMap,
	MDVisualizationTypeCluster,
	MDVisualizationTypePercentOverlay,
	MDVisualizationTypeSegues
} MDVisualizationType;

@protocol MDFunnelDelegate <NSObject>

-(void)funnel:(MDFunnel*)funnel changedImage:(UIImage*)newImage forFilterName:(NSString*)filterName;
-(void)funnel:(MDFunnel*)funnel changedFiltered:(NSArray*)filtered forFilterName:(NSString*)filterName;

@end

@interface MDFunnel : NSObject

@property(readwrite,nonatomic,strong) MDFilter *filter;

@property(readwrite,nonatomic,strong) NSArray *input;
@property(readonly,nonatomic,strong) NSDictionary<NSString*, NSArray*> *filtered;
@property(readonly,nonatomic,strong) NSDictionary<NSString*, UIImage*> *rendered;

@property(readwrite,nonatomic,weak) id<MDFunnelDelegate> delegate;

@property(readwrite,nonatomic) MDVisualizationType visualizationType;


-(NSArray*)filteredForFilterName:(NSString*)filterName;
-(UIImage*)imageForFilterName:(NSString*)filterName;

@end
