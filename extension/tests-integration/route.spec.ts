import { test, expect, chromium, type BrowserContext } from '@playwright/test';
import path from 'path';

const EXTENSION_PATH = path.join(__dirname, '..', 'dist');
const MOCK_COLLECTOR_URL = 'http://localhost:8787';

let context: BrowserContext;

test.beforeAll(async () => {
  context = await chromium.launchPersistentContext('', {
    headless: false,
    args: [
      `--disable-extensions-except=${EXTENSION_PATH}`,
      `--load-extension=${EXTENSION_PATH}`,
    ],
  });
});

test.afterAll(async () => {
  await context.close();
});

test('should capture route change events on Grafana mock', async () => {
  const page = await context.newPage();

  // Load mock Grafana site
  await page.goto(`file://${path.join(__dirname, '..', 'tools', 'mock-sites', 'grafana.html')}`);

  // Simulate navigation
  await page.evaluate(() => {
    window.history.pushState({}, '', '/d/test-uid/test-dashboard?var-region=us-east-1');
  });

  // Wait for event processing
  await page.waitForTimeout(2000);

  // Verify events were sent to collector
  const response = await fetch(`${MOCK_COLLECTOR_URL}/events`);
  const events = await response.json();

  expect(events.length).toBeGreaterThan(0);
  expect(events[0]).toMatchObject({
    platform: 'grafana',
    action: expect.stringMatching(/view|route_change/),
  });
});

test('should capture Datadog page views', async () => {
  const page = await context.newPage();

  await page.goto(`file://${path.join(__dirname, '..', 'tools', 'mock-sites', 'datadog.html')}`);

  await page.waitForTimeout(2000);

  const response = await fetch(`${MOCK_COLLECTOR_URL}/events`);
  const events = await response.json();

  const ddEvent = events.find((e: any) => e.platform === 'datadog');
  expect(ddEvent).toBeDefined();
});
