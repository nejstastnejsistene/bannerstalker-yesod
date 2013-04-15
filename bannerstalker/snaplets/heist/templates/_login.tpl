<form method="post" action="/login">
  <h1>Login</h1>
  <p>Not Registered? <a href="/register">Sign up now!</a></p>
  <p><loginError/></p>
  <label for="login">Email</label>
  <input type="text" id="login" name="login" placeholder="Email">
  <label for="password">Password</label>
  <input type="password" id="password" name="password" placeholder="Password">
  <button type="submit">Login</button>
  <p><a href="">Forget your password?</a></p>
</form>
