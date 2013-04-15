<apply template="base">
  <form method="post" action="/register">
    <h1>Register</h1>
    <p>Already registered? <a href="/login">Login instead.</a></p>
    <label for="email">Email</label>
    <input type="text" id="email" name="email" placeholder="Email">
    <label for="password">Password</label>
    <input type="password" id="password" name="password" placeholder="Password">
    <label for="confirm">Confirm password</label>
    <input type="password" id="confirm" name="confirm" placeholder="Password">
    <label class="item-label">
      <input type="checkbox">
      I have read and agree to the <a href="">Terms and Conditions of Use</a>.
      </label>
    <button type="submit">Signup</button>
  </form>
</apply>
