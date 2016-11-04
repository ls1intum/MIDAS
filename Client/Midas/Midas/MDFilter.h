//
//  MDFilter.h
//  Midas
//
//  Created by Thomas Günzel on 08/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

@import UIKit;

@class MDFilter;

@protocol MDFilterUpdateDelegate <NSObject>

-(void)filterChanged:(MDFilter*)filter forFilterName:(NSString*)filterName;

@end

@interface MDFilter : NSObject

-(instancetype)initWithName:(NSString*)name image:(UIImage*)image;

@property(readwrite,nonatomic,strong) NSString *name;
@property(readwrite,nonatomic,strong) UIImage *image;

@property(readwrite,nonatomic,weak) id<MDFilterUpdateDelegate> delegate;

-(BOOL)activeForFilterNamed:(NSString*)filterName;

-(void)resetForFilterNamed:(NSString*)filterName;

-(BOOL)matchesRecording:(id)recording forFilterNamed:(NSString*)filterName;

@end
