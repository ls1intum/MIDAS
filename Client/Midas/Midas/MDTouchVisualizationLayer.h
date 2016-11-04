//
//  MDTouchVisualizationLayer.h
//  Midas
//
//  Created by Thomas Günzel on 29/07/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <QuartzCore/QuartzCore.h>

@class MDTouchSequence;

@interface MDTouchVisualizationLayer : CALayer

@property(readwrite,nonatomic,strong) NSArray<MDTouchSequence*> *points;

@end
