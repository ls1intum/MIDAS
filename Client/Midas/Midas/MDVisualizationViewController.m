//
//  MDVisualizationViewController.m
//  Midas
//
//  Created by Thomas Günzel on 20/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "MDVisualizationViewController.h"

#import "MDDownloadedTouches.h"
#import "MDDownloadedActions.h"

#import "MDVisualizationView.h"
#import "MDSegueViewController.h"
#import "MDSegmentedControl.h"
#import "MDFilterViewController.h"

#import "MDFBNavigationView.h"

#import "MDFunnel.h"

#import "MDWindow.h"

#import "UIButton+MDCircleButton.h"
#import "UIViewController+Midas.h"

#import "MDPercentageOverlayView.h"

#import "MDScreenshotCache.h"

#import "UIImage+Midas.h"

@interface MDVisualizationViewController ()<MDDownloadedRecordingsDelegate,MDSegmentedControlDelegate,MDFunnelDelegate>

// Data Model
@property(readwrite,nonatomic,strong) MDFunnel *funnel;

// User Interface
@property(readwrite,nonatomic,strong) UIVisualEffectView *blurEffectView;
@property(readwrite,nonatomic,strong) UIView *contentView;

@property(readwrite,nonatomic,strong) UIView *backgroundSnapshotView;
@property(readwrite,nonatomic,strong) MDVisualizationView *containerView;
@property(readwrite,nonatomic,strong) MDSegueViewController *segueViewController;

@property(readwrite,nonatomic,strong) MDFilterViewController *filterViewController;
@property(readwrite,nonatomic,strong) MDFBNavigationView *navigationView;
@property(readwrite,nonatomic,strong) MDSegmentedControl *segmentedControl;

@property(readwrite,nonatomic,strong) UIButton *exitButton;

@property(readwrite,nonatomic,strong) UIButton *heatmapButton;
@property(readwrite,nonatomic,strong) UIButton *clusterButton;
@property(readwrite,nonatomic,strong) UIButton *percentButton;
@property(readwrite,nonatomic,strong) UIButton *segueButton;

@property(readwrite,nonatomic,strong) MDDownloadedTouches *downloadedTouches;
@property(readwrite,nonatomic,strong) MDDownloadedActions *downloadedActions;

@property(readwrite,nonatomic) MDVisualizationType visualizationType;

@end

@implementation MDVisualizationViewController

- (void)viewDidLoad {
    [super viewDidLoad];
	
	_funnel = [[MDFunnel alloc] init];
	_funnel.delegate = self;
	
	_visualizationType = MDVisualizationTypeHeatMap;
	
	//self.view.backgroundColor = [UIColor redColor];
	_containerView = [[MDVisualizationView alloc] initWithFrame:self.view.bounds];
	CGFloat scale = 0.72;
	_containerView.transform = CGAffineTransformScale(CGAffineTransformMakeTranslation(0.0, -70), scale, scale);
//	_containerView.hidden = YES;
	
	UIBlurEffect *blurEffect = [UIBlurEffect effectWithStyle:UIBlurEffectStyleDark];
	_blurEffectView = [[UIVisualEffectView alloc] initWithEffect:blurEffect];
	_contentView = _blurEffectView.contentView;
	_blurEffectView.frame = self.view.frame;
	_blurEffectView.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
	[self.view addSubview:_blurEffectView];
	
	[_blurEffectView.contentView addSubview:_containerView];
	
	[self setupSegmentedControl];
	[self setupFilterViewControl];
	[self setupSegueViewController];
	
	[self setupVisualizationButtons];
	[self setupExitButton];
	
	[self downloadTouches];
}

-(void)setupSegmentedControl {
	_segmentedControl = [[MDSegmentedControl alloc] init];
	UIColor *c1 = [UIColor colorWithRed:(57.0/255.0) green:(181.0/255.0) blue:(74.0/255.0) alpha:1.0];
	UIColor *c2 = [UIColor colorWithRed:(241.0/255.0) green:(90.0/255.0) blue:(36.0/255.0) alpha:1.0];
	UIColor *c3 = [UIColor colorWithRed:(0.0/255.0) green:(113.0/255.0) blue:(181.0/255.0) alpha:1.0];
	
	MDSegmentedControlItem *itm1 = [[MDSegmentedControlItem alloc] initWithTitle:@"All Touches" color:c1];
	MDSegmentedControlItem *itm2 = [[MDSegmentedControlItem alloc] initWithTitle:@"Filter #1" color:c2];
	MDSegmentedControlItem *itm3 = [[MDSegmentedControlItem alloc] initWithTitle:@"Filter #2" color:c3];
	_segmentedControl.rootItems = @[itm1, itm2, itm3];
	_segmentedControl.activeItemIndex = 1;
	_segmentedControl.translatesAutoresizingMaskIntoConstraints = NO;
	_segmentedControl.delegate = self;
	[_blurEffectView.contentView addSubview:_segmentedControl];
	
	
	[_segmentedControl.heightAnchor constraintEqualToConstant:44.0].active = YES;
	[_segmentedControl.bottomAnchor constraintEqualToAnchor:_blurEffectView.contentView.bottomAnchor].active = YES;
	[_segmentedControl.leadingAnchor constraintEqualToAnchor:_blurEffectView.contentView.leadingAnchor].active = YES;
	[_segmentedControl.trailingAnchor constraintEqualToAnchor:_blurEffectView.contentView.trailingAnchor].active = YES;
}

-(void)setupExitButton {
	_exitButton = [UIButton circleButtonWithTitle:@"Exit" image:[UIImage imageNamed:@"exit"]];
	_exitButton.translatesAutoresizingMaskIntoConstraints = NO;
	_exitButton.titleLabel.font = [UIFont systemFontOfSize:12.0];
	//_exitButton.borderWidth = 1.0;
	[_exitButton addTarget:self action:@selector(close:) forControlEvents:UIControlEventTouchUpInside];
	[_contentView addSubview:_exitButton];
	
	[_exitButton.widthAnchor constraintEqualToConstant:30.0].active = YES;
	[_exitButton.leadingAnchor constraintEqualToAnchor:_contentView.leadingAnchor constant:10.0].active = YES;
	[_exitButton.topAnchor constraintEqualToAnchor:_contentView.topAnchor constant:40.0].active = YES;
}

-(void)setupSegueViewController {
	_segueViewController = [[MDSegueViewController alloc] init];
	_segueViewController.funnel = _funnel;
//	_segueViewController.active = YES;
	
	UIView *segView = _segueViewController.view;
	
	[self addChildViewController:_segueViewController];
	segView.translatesAutoresizingMaskIntoConstraints = NO;
	segView.hidden = YES;
	[_contentView addSubview:_segueViewController.view];
	[_segueViewController didMoveToParentViewController:self];
	
	
	[segView.topAnchor constraintEqualToAnchor:_contentView.topAnchor constant:22.0].active = YES;
	[segView.bottomAnchor constraintEqualToAnchor:_filterViewController.view.topAnchor].active = YES;
	[segView.leftAnchor constraintEqualToAnchor:_contentView.leftAnchor].active = YES;
	[segView.rightAnchor constraintEqualToAnchor:_contentView.rightAnchor].active = YES;
}

-(void)setupFilterViewControl {
	_filterViewController = [[MDFilterViewController alloc] initWithFilter:_funnel.filter];
	_filterViewController.filterName = @"1";
	
	UIView *filterView = _filterViewController.view;
	filterView.translatesAutoresizingMaskIntoConstraints = NO;
	
	[self addChildViewController:_filterViewController];
	[_contentView addSubview:filterView];
	[_filterViewController didMoveToParentViewController:self];
	
	// add navigation buttons
	_navigationView = [[MDFBNavigationView alloc] init];
	_navigationView.translatesAutoresizingMaskIntoConstraints = NO;
	[_navigationView.backButton addTarget:_filterViewController action:@selector(back:) forControlEvents:UIControlEventTouchUpInside];
	[_navigationView.resetButton addTarget:_filterViewController action:@selector(reset:) forControlEvents:UIControlEventTouchUpInside];
	[_contentView addSubview:_navigationView];
	
	[_navigationView.widthAnchor constraintEqualToConstant:64.0].active = YES;
	[_navigationView.leadingAnchor constraintEqualToAnchor:_contentView.leadingAnchor].active = YES;
	[_navigationView.topAnchor constraintEqualToAnchor:filterView.topAnchor].active = YES;
	[_navigationView.bottomAnchor constraintEqualToAnchor:filterView.bottomAnchor].active = YES;
	[_navigationView.trailingAnchor constraintEqualToAnchor:filterView.leadingAnchor].active = YES;

	
	[filterView.heightAnchor constraintEqualToAnchor:_contentView.heightAnchor multiplier:0.28 constant:-(44.0+30.0)].active = YES;
//	[filterView.leadingAnchor constraintEqualToAnchor:_contentView.leadingAnchor].active = YES;
	[filterView.trailingAnchor constraintEqualToAnchor:_contentView.trailingAnchor].active = YES;
	[filterView.bottomAnchor constraintEqualToAnchor:_segmentedControl.topAnchor].active = YES;
	
//	[filterView.heightAnchor constraintEqualToAnchor:_navigationView.heightAnchor].active = YES;

}


-(void)setupVisualizationButtons {
	_heatmapButton = [UIButton circleButtonWithTitle:@"Heat" image:[UIImage midasImageNamed:@"visualization/heatmap"]];
	_heatmapButton.titleLabel.font = [UIFont systemFontOfSize:8.0];
	_heatmapButton.translatesAutoresizingMaskIntoConstraints = NO;
	[_heatmapButton addTarget:self action:@selector(changeVisualization:) forControlEvents:UIControlEventTouchUpInside];
	
	_clusterButton = [UIButton circleButtonWithTitle:@"Cluster" image:[UIImage midasImageNamed:@"visualization/cluster"]];
	_clusterButton.titleLabel.font = [UIFont systemFontOfSize:8.0];
	_clusterButton.translatesAutoresizingMaskIntoConstraints = NO;
	[_clusterButton addTarget:self action:@selector(changeVisualization:) forControlEvents:UIControlEventTouchUpInside];
	
	_percentButton = [UIButton circleButtonWithTitle:@"Percent" image:[UIImage midasImageNamed:@"visualization/percent"]];
	_percentButton.titleLabel.font = [UIFont systemFontOfSize:8.0];
	_percentButton.translatesAutoresizingMaskIntoConstraints = NO;
	[_percentButton addTarget:self action:@selector(changeVisualization:) forControlEvents:UIControlEventTouchUpInside];
	
	_segueButton = [UIButton circleButtonWithTitle:@"Segue" image:[UIImage midasImageNamed:@"visualization/segue"]];
	_segueButton.titleLabel.font = [UIFont systemFontOfSize:8.0];
	_segueButton.translatesAutoresizingMaskIntoConstraints = NO;
	[_segueButton addTarget:self action:@selector(changeVisualization:) forControlEvents:UIControlEventTouchUpInside];
	
	UIStackView *stack = [[UIStackView alloc] initWithArrangedSubviews:@[_heatmapButton,_clusterButton,_percentButton,_segueButton]];
	stack.distribution = UIStackViewDistributionFill;
	stack.axis = UILayoutConstraintAxisVertical;
	stack.alignment = UIStackViewAlignmentFill;
	stack.spacing = 30.0;
	stack.translatesAutoresizingMaskIntoConstraints = NO;
	
	[_contentView addSubview:stack];
	
	[stack.widthAnchor constraintEqualToConstant:32.0].active = YES;
//	[_heatmapButton.heightAnchor constraintEqualToAnchor:_exitButton.heightAnchor multiplier:1.0].active = YES;
	[_contentView.trailingAnchor constraintEqualToAnchor:stack.trailingAnchor constant:10.0].active = YES;
	[stack.topAnchor constraintEqualToAnchor:_contentView.topAnchor constant:40.0].active = YES;
}

-(void)segmentedControl:(MDSegmentedControl *)control didSelectItem:(MDSegmentedControlItem *)item atIndex:(NSUInteger)itemIndex {
	NSString *filterName = nil;
	if(itemIndex == 1) {
		filterName = @"1";
	} else if(itemIndex == 2) {
		filterName = @"2";
	}
	_filterViewController.filterName = filterName;
	_segueViewController.currentFilter = filterName;
	[self updateOverlay];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

-(void)viewWillLayoutSubviews {
	[super viewWillLayoutSubviews];
	
	[_exitButton centerVertically];
	[_heatmapButton centerVertically];
	[_clusterButton centerVertically];
	[_percentButton centerVertically];
	[_segueButton centerVertically];
}

-(void)setTargetViewController:(UIViewController *)targetViewController {
	if(_targetViewController == targetViewController) {
		return;
	}
	
	self.containerView.underlyingView = nil;
	_targetViewController = targetViewController;
	
//	[self downloadTouches];
//	[self downloadActions];
	
	//
	if(_targetViewController == nil) {
		return;
	}
	//[self addChildViewController:_targetViewController];
	if(self.viewIfLoaded == nil) {
		[self loadViewIfNeeded];
	}
	
	UIView *snapshotA = [_targetViewController.view snapshotViewAfterScreenUpdates:YES];
	UIView *snapshotB = [_targetViewController.view snapshotViewAfterScreenUpdates:YES];
	
	MDScreenshotCache *cache = [MDScreenshotCache sharedCache];
	NSString *identifier = _targetViewController.md_identifier;
	if([_targetViewController isKindOfClass:[UINavigationController class]]) {
		UINavigationController *t = (UINavigationController*)_targetViewController;
		identifier = t.topViewController.md_identifier;
	}
	if(identifier && [cache hasScreenshot:identifier] == NO) {
		[cache save:targetViewController.view asScreenshot:identifier];
	}
	self.containerView.underlyingView = snapshotA;
//	self.containerView.underlyingView = [_targetViewController.view snapshotViewAfterScreenUpdates:NO];
//	[_targetViewController didMoveToParentViewController:self];
	
	// Generate Snapshot
	
	self.backgroundSnapshotView = snapshotB;
}

-(void)setBackgroundSnapshotView:(UIView *)backgroundSnapshotView {
	if(_backgroundSnapshotView == backgroundSnapshotView) {
		return;
	}
	
	[_backgroundSnapshotView removeFromSuperview];
	_backgroundSnapshotView = backgroundSnapshotView;
	_backgroundSnapshotView.frame = self.view.bounds;
	_backgroundSnapshotView.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
	[self.view insertSubview:_backgroundSnapshotView atIndex:0];
}

-(void)downloadTouches {
	if(_downloadedTouches != nil) {
		return;
	}
	NSString *identifier = _targetViewController.md_namedViewController.md_identifier;
	if(identifier == nil) {
		return;
	}
	if([_targetViewController isKindOfClass:[UINavigationController class]]) {
		UINavigationController *t = (UINavigationController*)_targetViewController;
		identifier = t.topViewController.md_identifier;
	}
	if(identifier != nil) {
		_downloadedTouches = [[MDDownloadedTouches alloc] initForView:identifier];
		_downloadedTouches.delegate = self;
		[_downloadedTouches load];
	}
}

-(void)downloadActions {
	if(_downloadedActions != nil) {
		return;
	}
	NSString *identifier = _targetViewController.md_namedViewController.md_identifier;
	if(identifier == nil) {
		return;
	}
	if([_targetViewController isKindOfClass:[UINavigationController class]]) {
		UINavigationController *t = (UINavigationController*)_targetViewController;
		identifier = t.topViewController.md_identifier;
	}
	if(identifier != nil) {
		_downloadedActions = [[MDDownloadedActions alloc] initForView:identifier];
		_downloadedActions.delegate = self;
		[_downloadedActions load];
	}
}

-(void)recordings:(NSArray *)allRecordings didDownload:(id)downloader {
	if(downloader == _downloadedTouches) {
		_funnel.input = allRecordings;
		[self updateOverlay];
	} else if(downloader == _downloadedActions) {
		_funnel.input = allRecordings;
		[self updateOverlay];
//		_containerView.overlayImage = [_funnel imageForFilterName:_filterViewController.filterName];
	}
}

-(UIStatusBarStyle)preferredStatusBarStyle {
	return UIStatusBarStyleLightContent;
}

-(void)close:(id)sender {
	MDWindow *window = (MDWindow*)self.view.window;
	window.visualizingMode = NO;
}

-(void)changeVisualization:(id)sender {
	if(sender == _heatmapButton) {
		self.visualizationType = MDVisualizationTypeHeatMap;
	} else if(sender == _clusterButton) {
		self.visualizationType = MDVisualizationTypeCluster;
	} else if(sender == _percentButton) {
		self.visualizationType = MDVisualizationTypePercentOverlay;
	} else if(sender == _segueButton) {
		self.visualizationType = MDVisualizationTypeSegues;
	}
}

-(void)setVisualizationType:(MDVisualizationType)visualizationType {
	if(_visualizationType == visualizationType) {
		return;
	}
	
	_visualizationType = visualizationType;
	
	// change UI
	_segueViewController.active = NO;
	_segueViewController.view.hidden = YES;
	_containerView.hidden = YES;
	_funnel.delegate = self;
	
	_funnel.visualizationType = _visualizationType;
	
	switch (_visualizationType) {
		case MDVisualizationTypeCluster:
		case MDVisualizationTypeHeatMap:
			[self downloadTouches];
			_funnel.input = self.downloadedTouches.allRecordings;
			_containerView.hidden = NO;
			[self updateOverlay];
			break;
		case MDVisualizationTypePercentOverlay:
			[self downloadActions];
			_funnel.input = self.downloadedActions.allRecordings;
			_containerView.hidden = NO;
			[self updateOverlay];
			break;
		case MDVisualizationTypeSegues:
			_segueViewController.active = YES;
			_segueViewController.view.hidden = NO;
			break;
		default:
			break;
	}
	
}

//-(BOOL)prefersStatusBarHidden {
//	return YES;
//}


-(void)funnel:(MDFunnel *)funnel changedImage:(UIImage *)newImage forFilterName:(NSString *)filterName {
	//if([filterName isEqualToString:_filterViewController.filterName]) {
		[self updateOverlay];
	//}
}

-(void)funnel:(MDFunnel *)funnel changedFiltered:(NSArray *)filtered forFilterName:(NSString *)filterName {
	if(_funnel.visualizationType == MDVisualizationTypeHeatMap || _funnel.visualizationType == MDVisualizationTypeCluster) {
		return;
	}
	[self updateOverlay];
}

-(void)updateOverlay {
	if(_funnel.visualizationType == MDVisualizationTypeHeatMap || _funnel.visualizationType == MDVisualizationTypeCluster) {
		if([_containerView.overlayView isKindOfClass:[UIImageView class]] == NO) {
			_containerView.overlayView = [[UIImageView alloc] initWithFrame:_containerView.bounds];
		}
		
		UIImageView *imgView = ((UIImageView*)_containerView.overlayView);
		imgView.image = [_funnel imageForFilterName:_filterViewController.filterName];
	} else if(_funnel.visualizationType == MDVisualizationTypePercentOverlay) {
		if([_containerView.overlayView isKindOfClass:[MDPercentageOverlayView class]] == NO) {
			MDPercentageOverlayView *overlayView = [[MDPercentageOverlayView alloc] initWithFrame:_containerView.bounds];
			_containerView.overlayView = overlayView;
			
			overlayView.underlyingViewController = self.targetViewController.md_namedViewController;
		}
		
		MDPercentageOverlayView *v = (MDPercentageOverlayView*)_containerView.overlayView;
		v.actions = [_funnel filteredForFilterName:_filterViewController.filterName];
	}
}

@end
