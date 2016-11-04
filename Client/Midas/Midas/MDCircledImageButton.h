//
//  MDCircledImageButton.h
//  MidasVisualizeUI
//
//  Created by Thomas Günzel on 02/08/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface MDCircledImageButton : UIControl

-(instancetype)initWithTitle:(NSString*)title image:(UIImage*)image;

@property(readonly,nonatomic,strong) UILabel *label;
@property(readwrite,nonatomic,strong) NSString *title;
@property(readwrite,nonatomic,strong) UIImage *image;

@property(readwrite,nonatomic) CGFloat borderWidth;

@end
