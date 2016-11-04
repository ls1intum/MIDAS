//
//  SettingsViewController.m
//  MidasDemo
//
//  Created by Thomas Günzel on 27/06/16.
//  Copyright © 2016 Thomas Günzel. All rights reserved.
//

#import "SettingsViewController.h"

@interface SettingsViewController ()
@property (weak, nonatomic) IBOutlet UISwitch *showTitleSwitch;
@property (weak, nonatomic) IBOutlet UIButton *saveButton;

@end

@implementation SettingsViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view.
	self.showTitleSwitch.on = [[NSUserDefaults standardUserDefaults] boolForKey:@"showTitle"];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}
- (IBAction)switchAction:(id)sender {
	
}

-(UIStatusBarStyle)preferredStatusBarStyle {
	return UIStatusBarStyleLightContent;
}

/*
#pragma mark - Navigation

// In a storyboard-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender {
    // Get the new view controller using [segue destinationViewController].
    // Pass the selected object to the new view controller.
}
*/
- (IBAction)saveAndDismiss:(id)sender {
	[[NSUserDefaults standardUserDefaults] setBool:[_showTitleSwitch isOn] forKey:@"showTitle"];
	[self dismissViewControllerAnimated:YES completion:nil];
}

@end
