// SPDX-License-Identifier: GPL-2.0
/*
 * Copyright (C) 2019, Fabien Poussin
 * Based on Maxime Ripard's ILI9881C module
 */

#include <linux/backlight.h>
#include <linux/delay.h>
#include <linux/device.h>
#include <linux/err.h>
#include <linux/errno.h>
#include <linux/fb.h>
#include <linux/kernel.h>
#include <linux/module.h>

#include <linux/gpio/consumer.h>
#include <linux/regulator/consumer.h>

#include <drm/drm_mipi_dsi.h>
#include <drm/drm_modes.h>
#include <drm/drm_panel.h>

#include <video/mipi_display.h>

struct wf50dsya3mnn0 {
	struct drm_panel	panel;
	struct mipi_dsi_device	*dsi;

	struct backlight_device *backlight;
	struct regulator	*power;
	struct gpio_desc	*reset;
};

enum wf50dsya3mnn0_op {
	WF50DSYA3MNN0_SWITCH_PAGE,
	WF50DSYA3MNN0_COMMAND,
};

struct wf50dsya3mnn0_instr {
	enum wf50dsya3mnn0_op	op;

	union arg {
		struct cmd {
			u8	cmd;
			u8	data;
		} cmd;
		u8	page;
	} arg;
};

#define DSI_SWITCH_PAGE(_page)	\
	{					\
		.op = WF50DSYA3MNN0_SWITCH_PAGE,	\
		.arg = {			\
			.page = (_page),	\
		},				\
	}

#define DSI_COMMAND(_cmd, _data)		\
	{						\
		.op = WF50DSYA3MNN0_COMMAND,		\
		.arg = {				\
			.cmd = {			\
				.cmd = (_cmd),		\
				.data = (_data),	\
			},				\
		},					\
	}

static const struct wf50dsya3mnn0_instr wf50dsya3mnn0_init[] = {

  DSI_COMMAND(0x00, 0x00), // Noop

  DSI_SWITCH_PAGE(3),

  //GIP_1
  DSI_COMMAND(0x02, 0x00),
  DSI_COMMAND(0x03, 0x73),
  DSI_COMMAND(0x04, 0x00),
  DSI_COMMAND(0x05, 0x00),
  DSI_COMMAND(0x06, 0x0A),
  DSI_COMMAND(0x07, 0x00),
  DSI_COMMAND(0x08, 0x00),
  DSI_COMMAND(0x09, 0x01),
  DSI_COMMAND(0x0A, 0x00),
  DSI_COMMAND(0x0B, 0x00),
  DSI_COMMAND(0x0C, 0x01),
  DSI_COMMAND(0x0D, 0x00),
  DSI_COMMAND(0x0E, 0x00),
  DSI_COMMAND(0x0F, 0x1D),
  DSI_COMMAND(0x10, 0x1D),
  DSI_COMMAND(0x11, 0x00),
  DSI_COMMAND(0x12, 0x00),
  DSI_COMMAND(0x13, 0x00),
  DSI_COMMAND(0x14, 0x00),
  DSI_COMMAND(0x15, 0x00),
  DSI_COMMAND(0x16, 0x00),
  DSI_COMMAND(0x17, 0x00),
  DSI_COMMAND(0x18, 0x00),
  DSI_COMMAND(0x19, 0x00),
  DSI_COMMAND(0x1A, 0x00),
  DSI_COMMAND(0x1B, 0x00),
  DSI_COMMAND(0x1C, 0x00),
  DSI_COMMAND(0x1D, 0x00),
  DSI_COMMAND(0x1E, 0x40),
  DSI_COMMAND(0x1F, 0x80),
  DSI_COMMAND(0x20, 0x06),
  DSI_COMMAND(0x21, 0x02),
  DSI_COMMAND(0x22, 0x00),
  DSI_COMMAND(0x23, 0x00),
  DSI_COMMAND(0x24, 0x00),
  DSI_COMMAND(0x25, 0x00),
  DSI_COMMAND(0x26, 0x00),
  DSI_COMMAND(0x27, 0x00),
  DSI_COMMAND(0x28, 0x33),
  DSI_COMMAND(0x29, 0x03),
  DSI_COMMAND(0x2A, 0x00),
  DSI_COMMAND(0x2B, 0x00),
  DSI_COMMAND(0x2C, 0x00),
  DSI_COMMAND(0x2D, 0x00),
  DSI_COMMAND(0x2E, 0x00),
  DSI_COMMAND(0x2F, 0x00),
  DSI_COMMAND(0x30, 0x00),
  DSI_COMMAND(0x31, 0x00),
  DSI_COMMAND(0x32, 0x00),
  DSI_COMMAND(0x33, 0x00),
  DSI_COMMAND(0x34, 0x04),
  DSI_COMMAND(0x35, 0x00),
  DSI_COMMAND(0x36, 0x00),
  DSI_COMMAND(0x37, 0x00),
  DSI_COMMAND(0x38, 0x3C),
  DSI_COMMAND(0x39, 0x00),
  DSI_COMMAND(0x3A, 0x40),
  DSI_COMMAND(0x3B, 0x40),
  DSI_COMMAND(0x3C, 0x00),
  DSI_COMMAND(0x3D, 0x00),
  DSI_COMMAND(0x3E, 0x00),
  DSI_COMMAND(0x3F, 0x00),
  DSI_COMMAND(0x40, 0x00),
  DSI_COMMAND(0x41, 0x00),
  DSI_COMMAND(0x42, 0x00),
  DSI_COMMAND(0x43, 0x00),
  DSI_COMMAND(0x44, 0x00),

  //GIP_2
  DSI_COMMAND(0x50, 0x01),
  DSI_COMMAND(0x51, 0x23),
  DSI_COMMAND(0x52, 0x45),
  DSI_COMMAND(0x53, 0x67),
  DSI_COMMAND(0x54, 0x89),
  DSI_COMMAND(0x55, 0xAB),
  DSI_COMMAND(0x56, 0x01),
  DSI_COMMAND(0x57, 0x23),
  DSI_COMMAND(0x58, 0x45),
  DSI_COMMAND(0x59, 0x67),
  DSI_COMMAND(0x5A, 0x89),
  DSI_COMMAND(0x5B, 0xAB),
  DSI_COMMAND(0x5C, 0xCD),
  DSI_COMMAND(0x5D, 0xEF),

  //GIP_3
  DSI_COMMAND(0x5E, 0x11),
  DSI_COMMAND(0x5F, 0x01),
  DSI_COMMAND(0x60, 0x00),
  DSI_COMMAND(0x61, 0x15),
  DSI_COMMAND(0x62, 0x14),
  DSI_COMMAND(0x63, 0x0E),
  DSI_COMMAND(0x64, 0x0F),
  DSI_COMMAND(0x65, 0x0C),
  DSI_COMMAND(0x66, 0x0D),
  DSI_COMMAND(0x67, 0x06),
  DSI_COMMAND(0x68, 0x02),
  DSI_COMMAND(0x69, 0x07),
  DSI_COMMAND(0x6A, 0x02),
  DSI_COMMAND(0x6B, 0x02),
  DSI_COMMAND(0x6C, 0x02),
  DSI_COMMAND(0x6D, 0x02),
  DSI_COMMAND(0x6E, 0x02),
  DSI_COMMAND(0x6F, 0x02),
  DSI_COMMAND(0x70, 0x02),
  DSI_COMMAND(0x71, 0x02),
  DSI_COMMAND(0x72, 0x02),
  DSI_COMMAND(0x73, 0x02),
  DSI_COMMAND(0x74, 0x02),
  DSI_COMMAND(0x75, 0x01),
  DSI_COMMAND(0x76, 0x00),
  DSI_COMMAND(0x77, 0x14),
  DSI_COMMAND(0x78, 0x15),
  DSI_COMMAND(0x79, 0x0E),
  DSI_COMMAND(0x7A, 0x0F),
  DSI_COMMAND(0x7B, 0x0C),
  DSI_COMMAND(0x7C, 0x0D),
  DSI_COMMAND(0x7D, 0x06),
  DSI_COMMAND(0x7E, 0x02),
  DSI_COMMAND(0x7F, 0x07),
  DSI_COMMAND(0x80, 0x02),
  DSI_COMMAND(0x81, 0x02),
  DSI_COMMAND(0x82, 0x02),
  DSI_COMMAND(0x83, 0x02),
  DSI_COMMAND(0x84, 0x02),
  DSI_COMMAND(0x85, 0x02),
  DSI_COMMAND(0x86, 0x02),
  DSI_COMMAND(0x87, 0x02),
  DSI_COMMAND(0x88, 0x02),
  DSI_COMMAND(0x89, 0x02),
  DSI_COMMAND(0x8A, 0x02),

  DSI_SWITCH_PAGE(4),

  DSI_COMMAND(0x6C, 0x15),
  DSI_COMMAND(0x6E, 0x2B),
  DSI_COMMAND(0x6F, 0x33), // VGH & VGL OUTPUT

  DSI_COMMAND(0x8D, 0x18),
  DSI_COMMAND(0x87, 0xBA),
  DSI_COMMAND(0x26, 0x76),
  DSI_COMMAND(0xB2, 0xD1), // Reload Gamma setting

  DSI_COMMAND(0xB5, 0x06),
  DSI_COMMAND(0x3A, 0x24),
  DSI_COMMAND(0x35, 0x1F),

  DSI_SWITCH_PAGE(1),

  DSI_COMMAND(0x22, 0x09),
  DSI_COMMAND(0x31, 0x00), // Column inversion

  DSI_COMMAND(0x40, 0x33),
  DSI_COMMAND(0x53, 0xA2),
  DSI_COMMAND(0x55, 0x92),
  DSI_COMMAND(0x50, 0x96),
  DSI_COMMAND(0x51, 0x96),
  DSI_COMMAND(0x60, 0x22),
  DSI_COMMAND(0x61, 0x00),
  DSI_COMMAND(0x62, 0x19),
  DSI_COMMAND(0x63, 0x00),

  //---P-GAMMA START---
  DSI_COMMAND(0xA0, 0x08),
  DSI_COMMAND(0xA1, 0x11),
  DSI_COMMAND(0xA2, 0x19),
  DSI_COMMAND(0xA3, 0x0D),
  DSI_COMMAND(0xA4, 0x0D),
  DSI_COMMAND(0xA5, 0x1E),
  DSI_COMMAND(0xA6, 0x14),
  DSI_COMMAND(0xA7, 0x17),
  DSI_COMMAND(0xA8, 0x4F),
  DSI_COMMAND(0xA9, 0x1A),
  DSI_COMMAND(0xAA, 0x27),
  DSI_COMMAND(0xAB, 0x49),
  DSI_COMMAND(0xAC, 0x1A),
  DSI_COMMAND(0xAD, 0x18),
  DSI_COMMAND(0xAE, 0x4C),
  DSI_COMMAND(0xAF, 0x22),
  DSI_COMMAND(0xB0, 0x27),
  DSI_COMMAND(0xB1, 0x4B),
  DSI_COMMAND(0xB2, 0x60),
  DSI_COMMAND(0xB3, 0x39),
  //--- P-GAMMA END---

  //--- N-GAMMA START---
  DSI_COMMAND(0xC0, 0x08),
  DSI_COMMAND(0xC1, 0x11),
  DSI_COMMAND(0xC2, 0x19),
  DSI_COMMAND(0xC3, 0x0D),
  DSI_COMMAND(0xC4, 0x0D),
  DSI_COMMAND(0xC5, 0x1E),
  DSI_COMMAND(0xC6, 0x14),
  DSI_COMMAND(0xC7, 0x17),
  DSI_COMMAND(0xC8, 0x4F),
  DSI_COMMAND(0xC9, 0x1A),
  DSI_COMMAND(0xCA, 0x27),
  DSI_COMMAND(0xCB, 0x49),
  DSI_COMMAND(0xCC, 0x1A),
  DSI_COMMAND(0xCD, 0x18),
  DSI_COMMAND(0xCE, 0x4C),
  DSI_COMMAND(0xCF, 0x33),
  DSI_COMMAND(0xD0, 0x27),
  DSI_COMMAND(0xD1, 0x4B),
  DSI_COMMAND(0xD2, 0x60),
  DSI_COMMAND(0xD3, 0x39)
  //---N-GAMMA END---
};

static inline struct wf50dsya3mnn0 *panel_to_wf50dsya3mnn0(struct drm_panel *panel)
{
	return container_of(panel, struct wf50dsya3mnn0, panel);
}

/*
 * The panel seems to accept some private DCS commands that map
 * directly to registers.
 *
 * It is organised by page, with each page having its own set of
 * registers, and the first page looks like it's holding the standard
 * DCS commands.
 *
 * So before any attempt at sending a command or data, we have to be
 * sure if we're in the right page or not.
 */
static int wf50dsya3mnn0_switch_page(struct wf50dsya3mnn0 *ctx, u8 page)
{
	u8 buf[4] = { 0xff, 0x98, 0x81, page };
	int ret;

	ret = mipi_dsi_dcs_write_buffer(ctx->dsi, buf, sizeof(buf));
	if (ret < 0)
		return ret;

	return 0;
}

static int wf50dsya3mnn0_send_cmd_data(struct wf50dsya3mnn0 *ctx, u8 cmd, u8 data)
{
	u8 buf[2] = { cmd, data };
	int ret;

	ret = mipi_dsi_dcs_write_buffer(ctx->dsi, buf, sizeof(buf));
	if (ret < 0)
		return ret;

	return 0;
}

static int wf50dsya3mnn0_prepare(struct drm_panel *panel)
{
	struct wf50dsya3mnn0 *ctx = panel_to_wf50dsya3mnn0(panel);
	unsigned int i;
	int ret;

	/* Power the panel */
	ret = regulator_enable(ctx->power);
	if (ret)
		return ret;
	msleep(5);

	/* And reset it */
	gpiod_set_value(ctx->reset, 1);
	msleep(20);

	gpiod_set_value(ctx->reset, 0);
	msleep(20);

	for (i = 0; i < ARRAY_SIZE(wf50dsya3mnn0_init); i++) {
		const struct wf50dsya3mnn0_instr *instr = &wf50dsya3mnn0_init[i];

		if (instr->op == WF50DSYA3MNN0_SWITCH_PAGE)
			ret = wf50dsya3mnn0_switch_page(ctx, instr->arg.page);
		else if (instr->op == WF50DSYA3MNN0_COMMAND)
			ret = wf50dsya3mnn0_send_cmd_data(ctx, instr->arg.cmd.cmd,
						      instr->arg.cmd.data);

		if (ret)
			return ret;
	}

	ret = wf50dsya3mnn0_switch_page(ctx, 0);
	if (ret)
		return ret;

	ret = mipi_dsi_dcs_set_tear_on(ctx->dsi, MIPI_DSI_DCS_TEAR_MODE_VBLANK);
	if (ret)
		return ret;

	ret = mipi_dsi_dcs_exit_sleep_mode(ctx->dsi);
	if (ret)
		return ret;

	return 0;
}

static int wf50dsya3mnn0_enable(struct drm_panel *panel)
{
	struct wf50dsya3mnn0 *ctx = panel_to_wf50dsya3mnn0(panel);

	msleep(120);

	mipi_dsi_dcs_set_display_on(ctx->dsi);
	backlight_enable(ctx->backlight);

	return 0;
}

static int wf50dsya3mnn0_disable(struct drm_panel *panel)
{
	struct wf50dsya3mnn0 *ctx = panel_to_wf50dsya3mnn0(panel);

	backlight_disable(ctx->backlight);
	return mipi_dsi_dcs_set_display_off(ctx->dsi);
}

static int wf50dsya3mnn0_unprepare(struct drm_panel *panel)
{
	struct wf50dsya3mnn0 *ctx = panel_to_wf50dsya3mnn0(panel);

	mipi_dsi_dcs_enter_sleep_mode(ctx->dsi);
	regulator_disable(ctx->power);
	gpiod_set_value(ctx->reset, 1);

	return 0;
}

static const struct drm_display_mode panel_default_mode = {
	.clock		= 62000,
	.vrefresh	= 60,

	.hdisplay	= 720,
	.hsync_start	= 720 + 10,
	.hsync_end	= 720 + 10 + 20,
	.htotal		= 720 + 10 + 20 + 30,

	.vdisplay	= 1280,
	.vsync_start	= 1280 + 10,
	.vsync_end	= 1280 + 10 + 10,
	.vtotal		= 1280 + 10 + 10 + 20,
};

static int wf50dsya3mnn0_get_modes(struct drm_panel *panel)
{
	struct drm_connector *connector = panel->connector;
	struct wf50dsya3mnn0 *ctx = panel_to_wf50dsya3mnn0(panel);
	struct drm_display_mode *mode;

	mode = drm_mode_duplicate(panel->drm, &panel_default_mode);
	if (!mode) {
		dev_err(&ctx->dsi->dev, "failed to add mode %ux%ux@%u\n",
			panel_default_mode.hdisplay,
			panel_default_mode.vdisplay,
			panel_default_mode.vrefresh);
		return -ENOMEM;
	}

	drm_mode_set_name(mode);

	mode->type = DRM_MODE_TYPE_DRIVER | DRM_MODE_TYPE_PREFERRED;
	drm_mode_probed_add(connector, mode);

	panel->connector->display_info.width_mm = 62;
	panel->connector->display_info.height_mm = 110;

	return 1;
}

static const struct drm_panel_funcs wf50dsya3mnn0_funcs = {
	.prepare	= wf50dsya3mnn0_prepare,
	.unprepare	= wf50dsya3mnn0_unprepare,
	.enable		= wf50dsya3mnn0_enable,
	.disable	= wf50dsya3mnn0_disable,
	.get_modes	= wf50dsya3mnn0_get_modes,
};

static int wf50dsya3mnn0_dsi_probe(struct mipi_dsi_device *dsi)
{
	struct device_node *np;
	struct wf50dsya3mnn0 *ctx;
	int ret;

	ctx = devm_kzalloc(&dsi->dev, sizeof(*ctx), GFP_KERNEL);
	if (!ctx)
		return -ENOMEM;
	mipi_dsi_set_drvdata(dsi, ctx);
	ctx->dsi = dsi;

	drm_panel_init(&ctx->panel);
	ctx->panel.dev = &dsi->dev;
	ctx->panel.funcs = &wf50dsya3mnn0_funcs;

	ctx->power = devm_regulator_get(&dsi->dev, "power");
	if (IS_ERR(ctx->power)) {
		dev_err(&dsi->dev, "Couldn't get our power regulator\n");
		return PTR_ERR(ctx->power);
	}

	ctx->reset = devm_gpiod_get(&dsi->dev, "reset", GPIOD_OUT_LOW);
	if (IS_ERR(ctx->reset)) {
		dev_err(&dsi->dev, "Couldn't get our reset GPIO\n");
		return PTR_ERR(ctx->reset);
	}

	np = of_parse_phandle(dsi->dev.of_node, "backlight", 0);
	if (np) {
		ctx->backlight = of_find_backlight_by_node(np);
		of_node_put(np);

		if (!ctx->backlight)
			return -EPROBE_DEFER;
	}

	ret = drm_panel_add(&ctx->panel);
	if (ret < 0)
		return ret;

	dsi->mode_flags = MIPI_DSI_MODE_VIDEO_SYNC_PULSE;
	dsi->format = MIPI_DSI_FMT_RGB888;
	dsi->lanes = 4;

	return mipi_dsi_attach(dsi);
}

static int wf50dsya3mnn0_dsi_remove(struct mipi_dsi_device *dsi)
{
	struct wf50dsya3mnn0 *ctx = mipi_dsi_get_drvdata(dsi);

	mipi_dsi_detach(dsi);
	drm_panel_remove(&ctx->panel);

	if (ctx->backlight)
		put_device(&ctx->backlight->dev);

	return 0;
}

static const struct of_device_id wf50dsya3mnn0_of_match[] = {
	{ .compatible = "winstar,wf50dsya3mnn0" },
	{ }
};
MODULE_DEVICE_TABLE(of, wf50dsya3mnn0_of_match);

static struct mipi_dsi_driver wf50dsya3mnn0_dsi_driver = {
	.probe		= wf50dsya3mnn0_dsi_probe,
	.remove		= wf50dsya3mnn0_dsi_remove,
	.driver = {
		.name		= "wf50dsya3mnn0-dsi",
		.of_match_table	= wf50dsya3mnn0_of_match,
	},
};
module_mipi_dsi_driver(wf50dsya3mnn0_dsi_driver);

MODULE_AUTHOR("Fabien Poussin <fabien.poussin@gmail.com>");
MODULE_DESCRIPTION("Winstar WF50DSYA3MNN0 panel Driver");
MODULE_LICENSE("GPL v2");
