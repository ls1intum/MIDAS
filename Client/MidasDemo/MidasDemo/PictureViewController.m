//
//  PictureViewController.m
//  MidasDemo
//
//  Created by Thomas Günzel on 27/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "PictureViewController.h"
#import <WebImage/UIImageView+WebCache.h>

@import Midas;

@interface PictureViewController ()
@property (weak, nonatomic) IBOutlet UIImageView *imageView;
@property (weak, nonatomic) IBOutlet UILabel *titleLabel;

@end

@implementation PictureViewController

- (void)viewDidLoad {
    [super viewDidLoad];
	
	if([[NSUserDefaults standardUserDefaults] boolForKey:@"showTitle"] == YES) {
		self.titleLabel.text = [_imageJsonDictionary valueForKey:@"title"];
		self.titleLabel.hidden = NO;
	} else {
		self.titleLabel.hidden = YES;
	}
	
    // Do any additional setup after loading the view.
	NSURL *url = [NSURL URLWithString:[_imageJsonDictionary objectForKey:@"url"]];
	self.title = @"Loading...";
	[_imageView sd_setImageWithURL:url completed:^(UIImage *image, NSError *error, SDImageCacheType cacheType, NSURL *imageURL) {
		self.title = @"";
	}];
}

-(BOOL)md_excludeTitleFromIdentifier {
	return YES;
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

/*
#pragma mark - Navigation

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    // Get the new view controller using [segue destinationViewController].
    // Pass the selected object to the new view controller.
}
*/

@end
