import { expect, test } from '@playwright/test';

test('plays cards through the frontend and shows the backend board state', async ({ page }) => {
  await page.goto('/');

  await expect(page.getByText('Connection: open')).toBeVisible();
  await expect(page.getByText('Current player: player1')).toBeVisible();

  const player1 = page.getByTestId('player-player1');
  const hand = player1.getByTestId('hand-zone');
  const field = player1.getByTestId('field-zone');
  const graveyard = player1.getByTestId('graveyard-zone');

  await expect(hand.getByRole('heading', { name: 'Hand (5)' })).toBeVisible();
  await expect(hand.getByText('Edors Konstruct', { exact: true })).toBeVisible();
  await expect(hand.getByText('Energieladung', { exact: true })).toBeVisible();

  await page.getByPlaceholder('p1, c2, pass, end...').fill('p1');
  await page.getByRole('button', { name: 'Send' }).click();

  await expect(field.getByRole('heading', { name: 'Field (1)' })).toBeVisible();
  await expect(field.getByText('Edors Konstruct', { exact: true })).toBeVisible();
  await expect(field.getByText('STR 1000')).toBeVisible();
  await expect(hand.getByRole('heading', { name: 'Hand (4)' })).toBeVisible();

  await page.getByPlaceholder('p1, c2, pass, end...').fill('p1');
  await page.getByRole('button', { name: 'Send' }).click();
  await page.getByRole('button', { name: /Edors Konstruct/ }).first().click();

  await expect(field.getByRole('heading', { name: 'Field (1)' })).toBeVisible();
  await expect(field.getByText('Edors Konstruct', { exact: true })).toBeVisible();
  await expect(field.getByText('STR 3000')).toBeVisible();
  await expect(graveyard.getByRole('heading', { name: 'Graveyard (1)' })).toBeVisible();
  await expect(graveyard.getByText('Energieladung', { exact: true })).toBeVisible();
  await expect(hand.getByRole('heading', { name: 'Hand (3)' })).toBeVisible();

  await page.getByPlaceholder('p1, c2, pass, end...').fill('end');
  await page.getByRole('button', { name: 'Send' }).click();

  await expect(page.getByText('Current player: player2')).toBeVisible();
  await expect(field.getByText('STR 1000')).toBeVisible();
  await expect(graveyard.getByText('Energieladung', { exact: true })).toBeVisible();
});
